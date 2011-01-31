/**
 * Simple Key/Value store
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (C) 2011 Bob Jamison
 * 
 *  This file is part of the Pedro library.
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 3 of the License, or (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *  Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public
 *  License along with this library; if not, write to the Free Software
 *  Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
 */

package pedro.data

import java.lang.reflect.{Method,Modifier}


trait Data extends Product
{
    val id: String

    def toJson = Json.toJson(this).toString

    override def toString = toJson
}


/**
 * Indices are associated with a Kind, and are ways of indicating which fields
 * of the Kind data should be searchable. 
 */ 
abstract class Index[T<:Any](nam: String)(grab:(JsonValue)=>JsonValue)
{
    val name    = nam
    //Grab the JsonValue that serializes the indexed value
    val grabber = grab   
    //Convert the JsonValue to the indexed type
    def get(j:JsonValue) : T
}

/**
 * The types of indices we handle.  Note that JsonArrays of these will be handled the same way,
 * with the only difference being that a put() that involves an array index will simply
 * be spread among multiple index entries
 */   
class BooleanIndex(nam: String)(val grab:(JsonValue)=>JsonValue) extends Index[Boolean](nam)(grab)
{
    def get(j:JsonValue) : Boolean = j  //use inference!
}
class DoubleIndex (nam: String)(val grab:(JsonValue)=>JsonValue) extends Index[Double] (nam)(grab)
{
    def get(j:JsonValue) : Double = j
}
class IntIndex    (nam: String)(val grab:(JsonValue)=>JsonValue) extends Index[Int]    (nam)(grab)
{
    def get(j:JsonValue) : Int = j
}
class StringIndex (nam: String)(val grab:(JsonValue)=>JsonValue) extends Index[String] (nam)(grab)
{
    def get(j:JsonValue) : String = j
}

/**
 * This relates closely to a SQL table, or a Kind in BigTable
 */ 
class Kind[+T<:Data](val name: String)(jsToData:(JsonValue) => T)
{
    private val indx = scala.collection.mutable.ListBuffer[Index[_]]()

    def booleanIndex(nam: String)(grab:(JsonValue)=>JsonValue) =
        {
        val idx = new BooleanIndex(nam)(grab)
        indx += idx
        idx
        }
    
    def doubleIndex(nam: String)(grab:(JsonValue)=>JsonValue) =
        {
        val idx = new DoubleIndex(nam)(grab)
        indx += idx
        idx
        }
    
    def intIndex(nam: String)(grab:(JsonValue)=>JsonValue) =
        {
        val idx = new IntIndex(nam)(grab)
        indx += idx
        idx
        }
    
    def stringIndex(nam: String)(grab:(JsonValue)=>JsonValue) =
        {
        val idx = new StringIndex(nam)(grab)
        indx += idx
        idx
        }

    def indices = indx.toList

    //def toString(data: T) : String = Json.toJson(data).toString
    
    def fromJson(js: JsonValue) : T =
        jsToData(js)
    
    def fromString(str: String) : Option[T] =
        JsonParser.parse(str).map(jsToData)

}







trait Schema
{
    protected val kbuf = scala.collection.mutable.ListBuffer[Kind[Data]]()
    
    def add[T<:Data](kind: Kind[T]) : Kind[T] =
        {
        kbuf += kind
        kind
        }
    
    def kinds = kbuf.toList
    
}


/**
 * This defines all fields and methods that should be implemented in a 
 * KvStore implementation.
 */  
trait KvStore
{
    def connect : Boolean
    def disconnect : Boolean
    def create(schema: Schema) : Boolean
    def exists(name: String, id: String) : Boolean
    def put[T <: Data](kind: Kind[T], data: T) : Boolean
    def get[T <: Data](kind: Kind[T], id: String): Option[T]
    def list[T<:Data](kind: Kind[T]) : Option[Seq[T]]
    def query[T<:Data, U<:Any](kind: Kind[T], index:Index[U], comp: (U)=>Boolean) : Option[Seq[T]]
    def delete[T <: Data](kind: Kind[T], id: String): Boolean
}

object KvStoreType extends Enumeration
{
    type KvStoreType = Value
    val JDBC, Cassandra = Value
}

import KvStoreType._

object KvStore extends pedro.util.Logged
{
    def create(typ: KvStoreType, opts: (String,String)*) : Option[KvStore] =
	    {
		typ match
		    {
			case JDBC => Some(new JdbcKvStore(opts.toMap))
			case Cassandra => Some(new CassandraKvStore(opts.toMap))
			case _ => error("Unknown KvStore type: " + typ)
			    None
			}
		}
}


/**
 * A Key/Value store which uses a JDBC database as a backend.  This is possibly
 * not the most efficient implementation using a relational database, but it will
 * work when others are not available.
 * Note that the values to override are:  driver, url, user, pass.
 */
class JdbcKvStore(opts: Map[String, String] = Map())
    extends KvStore with pedro.util.Logged
{
    val jdbcDriver = opts.getOrElse("driver", "org.h2.Driver")
    val jdbcUrl    = opts.getOrElse("url", "jdbc:h2:pedro")
    val jdbcUser   = opts.getOrElse("user", "sa")
    val jdbcPass   = opts.getOrElse("pass", "")

    var conn : Option[java.sql.Connection] = None
    private def checkConnect =
        {
        if (conn.isEmpty || conn.get.isClosed)
            {
            if (!connect)
                { error("not connected") ; false }
            else
                true
            }
        else 
            true
        }
    
    def connect : Boolean =
         {
         try
             {
             Class.forName(jdbcDriver)
             conn = Some(java.sql.DriverManager.getConnection(jdbcUrl, jdbcUser, jdbcPass))
             true
             }
        catch
            {
            case e:Exception => error("connect: " + e)
                false
            }
         }

    def disconnect : Boolean =
         {
         try
             {
             if (conn.isDefined)
                  { conn.get.close ; conn = None }
             true
             }
        catch
            {
            case e:Exception => error("disconnect: " + e)
                false
            }
         }

    /**
     * Creates the data tables for the schema
     */         
    def create(schema: Schema) : Boolean =
        {
        if (!checkConnect) return false
        schema.kinds.foreach(k=>
            {
            try
                {
                val stmt = conn.get.prepareStatement("drop table " + k.name)
                stmt.execute
                trace(stmt.toString)
                }
            catch
                {
                case e:Exception => trace("drop: no table:" + e)
                }
            try
                {
                val stmt = conn.get.prepareStatement("create table " + k.name + 
                    " (id varchar(128) primary key, value text not null)")
                stmt.execute
                trace(stmt.toString)
                }
            catch
                {
                case e:Exception => error("create:" + e) ; return false
                }
            k.indices.foreach(i=>
                {
                val name = k.name + "_" + i.name
                try
                    {
                    val stmt = conn.get.prepareStatement("drop table " + name)
                    stmt.execute
                    trace(stmt.toString)
                    }
                catch
                    {
                    case e:Exception => trace("drop index:" + e)                  
                    }
                try
                    {
                    val stmt = i match
                        {
                        case v:BooleanIndex => conn.get.prepareStatement("create table " + name +
                             " (id varchar(128) not null, value boolean not null)")
                        case v:DoubleIndex  => conn.get.prepareStatement("create table " + name +
                             " (id varchar(128) not null, value float not null)")
                        case v:IntIndex     => conn.get.prepareStatement("create table " + name +
                             " (id varchar(128) not null, value integer not null)")
                        case v:StringIndex  => conn.get.prepareStatement("create table " + name +
                             " (id varchar(128) not null, value text not null)")
                        case _ => error("create: unknown index type") ; return false
                        }
                    stmt.execute
                    trace(stmt.toString)
                    }
                catch
                    {
                    case e:Exception => error("create index:" + e) ; return false                    
                    }
                })
            })
        true
        }
    
    /**
     * Puts a key/value pair into the table
     */         
    def exists(name: String, id: String) : Boolean =
        {
        if (!checkConnect) return false
        try
            {
            var count = 0
            val stmt = conn.get.prepareStatement("select count(*) from " + name + " where id=?")
            stmt.setString(1, id)
            val rs = stmt.executeQuery
            if (rs.next)
    	        count = rs.getInt(1)
            stmt.close
    	    count > 0
            }
        catch
            {
            case e:Exception => error("connect: " + e)
                false
            }
        }
    
    /**
     * Determines is a keyed value already exists
     */         
    def put[T <: Data](kind: Kind[T], data: T) : Boolean =
        {
        if (!checkConnect) return false
        try
            {
            val js = Json.toJson(data)
            trace("json: " + js.toString + "   indices:" + kind.indices.size)
            var sql = if (exists(kind.name, data.id))
                "update " + kind.name + " set value=? where id=?"
            else
                "insert into " + kind.name + " (value,id) values(?,?)"
            var stmt = conn.get.prepareStatement(sql)
            stmt.setString(1, js.toString)
            stmt.setString(2, data.id)
            trace("put:" + stmt.toString)
            val rs = stmt.executeUpdate
            stmt.close
            kind.indices.foreach(i=>
                {
                val name = kind.name + "_" + i.name
                trace("index: " + name)
                sql = "delete from " + name + " where id=?"
                stmt = conn.get.prepareStatement(sql)
                stmt.setString(1, data.id)
                trace("put delete index: " + stmt.toString)
                val rs = stmt.executeUpdate
                stmt.close
                val jsx = i.grabber(js) match
                    {
                    case v:JsonArray => v.toList
                    case _           => List(js)
                    }
                jsx.foreach(j=>
                    {
                    val v = i.get(j)
                    sql = "insert into " + name + " (id,value) values(?,?)"
                    stmt = conn.get.prepareStatement(sql)
                    stmt.setString(1, data.id)
                    stmt.setObject(2, v)
                    trace("put index: " + stmt.toString)
                    val rs = stmt.executeUpdate
                    stmt.close
                    })
                })
            true
            }
        catch
            {
            case e:Exception => error("connect: " + e)
                false
            }
        }
    
    def get[T <: Data](kind: Kind[T], id: String): Option[T] =
        {
        if (!checkConnect) return None
        try
            {
            val stmt = conn.get.prepareStatement("select value from " + kind.name + " where id=?")
            stmt.setString(1, id)
            val rs = stmt.executeQuery
            val res = if (rs.next)
                    kind.fromString(rs.getString(1))
                else
                    None
            stmt.close
            res
            }
        catch
            {
            case e:Exception => error("get: " + e)
                None
            }        
        }
    
    def list[T<:Data](kind: Kind[T]) : Option[Seq[T]] =
        {
        if (!checkConnect) return None
        try
            {
            val res = scala.collection.mutable.ListBuffer[T]()
            var stmt = conn.get.prepareStatement("select value from " + kind.name)
            var rs = stmt.executeQuery
            while (rs.next)
                {
                val s = rs.getString(1)
                val v = kind.fromString(s)
                if (v.isDefined) res.append(v.get)
                }
            stmt.close
            Some(res.toList)            
            }
        catch
            {
            case e:Exception => error("list: " + e + ":" + e.printStackTrace)
                None
            }        
        }


    def query[T<:Data, U<:Any](kind: Kind[T], index:Index[U], comp: (U)=>Boolean) : Option[Seq[T]] =
        {
        if (!checkConnect) return None
        try
            {
            val ids = scala.collection.mutable.ListBuffer[String]()
            val iname = kind.name + "_" + index.name
            var stmt = conn.get.prepareStatement("select id,value from " + iname)
            var rs = stmt.executeQuery
            while (rs.next)
                {
                index match
                    { //here is where comp() is used to filter values & their ids
                    case v:BooleanIndex    => if (comp(rs.getBoolean(2))) ids += rs.getString(1)
                    case v:DoubleIndex     => if (comp(rs.getDouble(2)))  ids += rs.getString(1)
                    case v:IntIndex        => if (comp(rs.getInt(2)))     ids += rs.getString(1)
                    case v:StringIndex     => if (comp(rs.getString(2)))  ids += rs.getString(1)
                    }
                }
            stmt.close
            val res = scala.collection.mutable.ListBuffer[T]()
            val sql = "select value from " + kind.name +
                 " where id in " + ids.map(_=>"?").mkString("(", ",",")")
            stmt = conn.get.prepareStatement(sql)
            ids.zipWithIndex.foreach(s=> stmt.setString(s._2+1, s._1))
            trace("query: " + stmt.toString)
            rs = stmt.executeQuery
            while (rs.next)
                {
                val s = rs.getString(1)
                println("s:" + s)
                val v = kind.fromString(s)
                if (v.isDefined) res.append(v.get)
                }
            stmt.close
            Some(res.toList)            
            }
        catch
            {
            case e:Exception => error("query: " + e + ":" + e.printStackTrace)
                None
            }        
        }


    def delete[T <: Data](kind: Kind[T], id: String): Boolean =
        {
        if (!checkConnect) return false
        try
            {
            var stmt = conn.get.prepareStatement("delete from " + kind.name + " where id=?")
            stmt.setString(1, id)
            val rs = stmt.execute
            stmt.close
            kind.indices.foreach(i=>
                {
                val name = kind.name + "_" + i.name
                stmt = conn.get.prepareStatement("delete from " + name + " where id=?")
                stmt.setString(1, id)
                val rs = stmt.execute
                stmt.close
                })
            true
            }
        catch
            {
            case e:Exception => error("delete: " + e)
                false
            }        
        }
    
}




//########################################################################
//# E N D
//########################################################################

