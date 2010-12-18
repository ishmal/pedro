/**
 * Simple Key/Value store
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (C) 2010 Bob Jamison
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


/**
 * This relates closely to a SQL table, or a Kind in BigTable
 */ 
class Kind[T<:Product](val name: String)(block:(JsonValue) => T)
{
    lazy val indices =
        {
        val methods = getClass.getMethods.map(m=>(m.getName, m)).toMap
        val fields  = getClass.getDeclaredFields.collect
            {case f if Modifier.isPrivate(f.getModifiers) => f.getName}.toSet
        val vals = methods.filterKeys(fields)
        vals.valuesIterator.map(_.invoke(this)).collect{case i: Index[_] => i}
        }

    def toString(data: T) : String = Json.toJson(data).toString
    
    def fromString(str: String) : Option[T] =
        {
        JsonParser.parse(str) match
            {
            case JsonSuccess(js) => Some(block(js))
            case _ => None
            }
        }
}



/**
 * Indices are associated with a Kind, and are ways of indicating which fields
 * of the Kind data should be searchable. 
 */ 
class Index[T<:Any](nam: String)(grab:(JsonValue)=>T)
{
    val name    = nam
    val grabber = grab
}


/**
 * The types of indices we handle.  Note that JsonArrays of these will be handled the same way,
 * with the only difference being that a put() that involves an array index will simply
 * be spread among multiple index entries
 */   
class BooleanIndex(nam: String)(val grab:(JsonValue)=>Boolean) extends Index[Boolean](nam)(grab)
class DoubleIndex (nam: String)(val grab:(JsonValue)=>Double)  extends Index[Double] (nam)(grab)
class IntIndex    (nam: String)(val grab:(JsonValue)=>Int)     extends Index[Int]    (nam)(grab)
class StringIndex (nam: String)(val grab:(JsonValue)=>String)  extends Index[String] (nam)(grab)



trait Schema
{

    lazy val kinds =
        {
        val methods = getClass.getMethods.map(m=>(m.getName, m)).toMap
        val fields  = getClass.getDeclaredFields.collect
            {case f if Modifier.isPrivate(f.getModifiers) => f.getName}.toSet
        val vals = methods.filterKeys(fields)
        vals.valuesIterator.map(_.invoke(this)).collect{case k: Kind[_] => k}
        }

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
    def put[T <: Product](kind: Kind[T], id: String, data: T) : Boolean
    def get[T <: Product](kind: Kind[T], id: String): Option[T]
    def query[T<:Product, U<:Any](kind: Kind[T], index:Index[U], comp: (U)=>Boolean) : Option[Seq[T]]
    def delete[T <: Product](kind: Kind[T], id: String): Boolean
}

class JdbcKvStore extends KvStore with pedro.util.Logged
{
    val jdbcUrl  = "jdbc:h2:pedro"
    val jdbcUser = "ishmal"
    val jdbcPass = "flamingo"
    var conn : Option[java.sql.Connection] = None
    

    private def checkConnect =
        {
        if (conn.isEmpty || conn.get.isClosed)
            { error("not connected") ; false }
        else 
            true
        }
    
    def connect : Boolean =
         {
         try
             {
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
            case e:Exception => error("connect: " + e)
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
            var stmt = conn.get.prepareStatement("drop table " + k.name)
            stmt.execute
            trace(stmt.toString)
            stmt = conn.get.prepareStatement("create table " + k.name + 
                " (id varchar(128) primary key, value text not null)")
            stmt.execute
            trace(stmt.toString)
            k.indices.foreach(i=>
                {
                val name = k.name + "_" + i.name
                stmt = conn.get.prepareStatement("drop table " + name)
                stmt.execute
                trace(stmt.toString)
                stmt = i match
                    {
                    case v:BooleanIndex => conn.get.prepareStatement("create table " + name +
                         " id varchar(128) not null, value boolean not null)")
                    case v:DoubleIndex  => conn.get.prepareStatement("create table " + name +
                         " id varchar(128) not null, value float not null)")
                    case v:IntIndex     => conn.get.prepareStatement("create table " + name +
                         " id varchar(128) not null, value integer not null)")
                    case v:StringIndex  => conn.get.prepareStatement("create table " + name +
                         " id varchar(128) not null, value text not null)")
                    case _ => error("create: unknown index type") ; return false
                    }
                stmt.execute
                trace(stmt.toString)
                })
            })
        true
        }
    
    /**
     * Determines if an id already exists in a table
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
    def put[T <: Product](kind: Kind[T], id: String, data: T) : Boolean =
        {
        if (!checkConnect) return false
        try
            {
            val js = Json.toJson(data)
            var sql = if (exists(kind.name, id))
                "update " + kind.name + " set value=? where id=?"
            else
                "insert into " + kind.name + " (value,id) values(?,?)"
            var stmt = conn.get.prepareStatement(sql)
            stmt.setString(1, js)
            stmt.setString(2, id)
            val rs = stmt.executeUpdate
            stmt.close
            kind.indices.foreach(i=>
                {
                val name = kind.name + "_" + i.name
                val jsx = js match
                    {
                    case v:JsonArray => v.toList
                    case _           => List(js)
                    }
                jsx.foreach(j=>
                    {
                    val v = i.grabber(j)
                    sql = if (exists(i.name, id))
                        "update " + i.name + " set value=? where id=?"
                    else
                        "insert into " + i.name + " (value,id) values(?,?)"
                    stmt = conn.get.prepareStatement(sql)
                    stmt.setObject(1, v)
                    stmt.setString(2, id)
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
    
    def get[T <: Product](kind: Kind[T], id: String): Option[T] =
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
    
    def query[T<:Product, U<:Any](kind: Kind[T], index:Index[U], comp: (U)=>Boolean) : Option[Seq[T]] =
        {
        try
            {
            val ids = scala.collection.mutable.ListBuffer[String]()
            var stmt = conn.get.prepareStatement("select id,value from " + index.name)
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
            val sql = "select value from " + kind.name + " where id in " + ids.mkString("('","','","')")
            stmt = conn.get.prepareStatement(sql)
            ids.zipWithIndex.foreach(s=>stmt.setString(s._2+1, s._1))
            rs = stmt.executeQuery
            while (rs.next)
                {
                val v = kind.fromString(rs.getString(1))
                if (v.isDefined) res.append(v.get)
                }
            stmt.close
            Some(res.toList)            
            }
        catch
            {
            case e:Exception => error("query: " + e)
                None
            }        
        }


    def delete[T <: Product](kind: Kind[T], id: String): Boolean =
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
//# T E S T
//########################################################################


case class User(
    val id : String = "",
    val name: String = ""
)
{}


object TestSchema extends Schema
{
    val users = new Kind[User]("users")(js=>
        {
        User(
            id=js("id"),
            name=js("name")
            )
        })
        {
        val users_name = new StringIndex("name")(js=>js("name"))
        }
}



object KVTest
{
    


    def doTest =
        {
        val store = new JdbcKvStore
        store.create(TestSchema)     
        }

    def main(argv: Array[String]) =
        doTest

}

//########################################################################
//# E N D
//########################################################################

