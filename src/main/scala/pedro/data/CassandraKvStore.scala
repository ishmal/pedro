/**
 * Simple Key/Value store.  Cassandra backend
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

import org.apache.cassandra.thrift.Cassandra

import org.apache.thrift.TException
import org.apache.thrift.transport.{TTransport,TFramedTransport,TSocket}
import org.apache.thrift.protocol.TBinaryProtocol
import org.apache.cassandra.thrift.{Column,ColumnParent,ColumnPath,ConsistencyLevel}
import org.apache.cassandra.thrift.{KeySlice,KeyRange,SliceRange,SlicePredicate}
import java.nio.ByteBuffer
  
/**
 * A Key/Value store which uses a Cassandra database as a backend.
 * Note that the values to override are:  driver, url, user, pass.
 */
class CassandraKvStore(opts: Map[String, String] = Map())
    extends KvStore with pedro.util.Logged
{
    val host     = opts.getOrElse("host", "127.0.0.1")
    val port     = opts.getOrElse("port", "9160").toInt
    val user     = opts.getOrElse("user", "")
    val pass     = opts.getOrElse("pass", "")
	val keyspace = opts.getOrElse("keyspace", "Keyspace1")

	class Connection(val cli: Cassandra.Client, val transport: TTransport)
	    {
		def isOpen = transport.isOpen
		def close = transport.close
		}

    var conn : Option[Connection] = None
    private def checkConnect =
        {
        if (conn.isEmpty || !conn.get.isOpen)
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
            val tr = new TFramedTransport(new TSocket(host, port))
            val proto = new TBinaryProtocol(tr)
            val client = new Cassandra.Client(proto)
            tr.open
		    client.set_keyspace(keyspace);
			conn = Some(new Connection(client, tr))
			true
            }
        catch
            {
            case e:Exception => error("connect: " + e)
			    conn = None
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
            })
        true
        }
    
    /**
     * Puts a key/value pair into the table
     */         
    def put[T <: Data](kind: Kind[T], data: T) : Boolean =
        {
        if (!checkConnect) return false
        try
		    {
			val js = Json.toJson(data)
			conn.get.cli.insert(ByteBuffer.wrap(data.id.getBytes),
                new ColumnParent(kind.name),
                new Column(ByteBuffer.wrap("value".getBytes("UTF-8")),
                    ByteBuffer.wrap(js.toString.getBytes("UTF-8")),
                    System.currentTimeMillis),
                ConsistencyLevel.ONE)
            kind.indices.foreach(i=>
                {
                val name = kind.name + "_" + i.name
                trace("index: " + name)
                val jsx = i.grabber(js) match
                    {
                    case v:JsonArray => v.toList
                    case _           => List(js)
                    }
                jsx.foreach(j=>
                    {
                    val v = i.get(j)
                    conn.get.cli.insert(ByteBuffer.wrap(data.id.getBytes),
                        new ColumnParent(name),
                        new Column(ByteBuffer.wrap("value".getBytes("UTF-8")),
                            ByteBuffer.wrap(js.toString.getBytes("UTF-8")),
                            System.currentTimeMillis),
                        ConsistencyLevel.ONE)
                    })
                })
			true
			}
		catch
		    {
			case e: Exception => error("put: " + e)
			    false
			}
        }
    
    def get[T <: Data](kind: Kind[T], id: String): Option[T] =
        {
        if (!checkConnect) return None
		val cp = new ColumnPath(kind.name)
		try
		    {
			val col = conn.get.cli.get(ByteBuffer.wrap(id.getBytes("UTF-8")), cp, ConsistencyLevel.ONE)
			val value = new String(col.getColumn.getValue)
			kind.fromString(value)
			}
		catch
		    {
			case e:Exception => error("get: " + e)
			    None
			}
        }
    
	val allKeyRange =
	    {
		val keyRange = new KeyRange
        keyRange.setStart_key(Array[Byte]())
        keyRange.setEnd_key(Array[Byte]())
		keyRange
		}
	
	val allSlicePredicate =
	    {
		val slicePredicate = new SlicePredicate
        val sliceRange = new SliceRange
	    sliceRange.setStart(Array[Byte]())
        sliceRange.setFinish(Array[Byte]())  
        slicePredicate.setSlice_range(sliceRange)
		slicePredicate
		}
        
    def longToBytes(lval: Long ) =
        Array[Byte]( ((lval >> 24)&0xff).toByte, ((lval >> 16)&0xff).toByte,
                     ((lval >>  8)&0xff).toByte, ((lval      )&0xff).toByte )

    def doubleToBytes(d: Double) =
        longToBytes(java.lang.Double.doubleToRawLongBits(d))
	
    def bytesToLong(arr: Array[Byte]) =
        arr.foldLeft(0L){ (lval, b) => (lval << 8) | (b.toLong & 0xff) }
        
    def bytesToDouble(arr: Array[Byte]) =
        java.lang.Double.longBitsToDouble(bytesToLong(arr))
	
	import scala.collection.JavaConversions._
	
    def list[T<:Data](kind: Kind[T]) : Option[Seq[T]] =
        {
        if (!checkConnect) return None
		try
		    {
			val keySlices = List[KeySlice]() ++ conn.get.cli.get_range_slices(new ColumnParent(kind.name),
      			allSlicePredicate, allKeyRange, ConsistencyLevel.ONE)
			val res = keySlices.map(ks=> kind.fromString(new String(ks.getColumns()(0).column.getValue)))
			Some(res.flatten)
			}
		catch
		    {
			case e:Exception => error("get: " + e)
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
            val indexSlices = List() ++ conn.get.cli.get_range_slices(new ColumnParent(kind.name),
        		allSlicePredicate, allKeyRange, ConsistencyLevel.ONE)
            for (slice <- indexSlices)
			    {
                val key = new String(slice.getColumns()(0).column.getValue)
                val vb = slice.getColumns()(1).column.getValue
                index match
                    { //here is where comp() is used to filter values & their ids
                    case v:BooleanIndex => val bv = if (vb(0)==0) false else true ; if (comp(bv)) ids += key
                    case v:DoubleIndex  => val dv = bytesToDouble(vb); if (comp(dv)) ids += key
                    case v:LongIndex    => val iv = bytesToLong(vb);   if (comp(iv)) ids += key
                    case v:StringIndex  => val sv = new String(vb);    if (comp(sv)) ids += key
                    }
                }
			val keySlices = List[KeySlice]() ++ conn.get.cli.get_range_slices(new ColumnParent(kind.name),
        		allSlicePredicate, allKeyRange, ConsistencyLevel.ONE)
			val res = keySlices.map(ks=> kind.fromString(new String(ks.getColumns()(0).column.getValue)))
			Some(res.flatten)
			}
		catch
		    {
			case e:Exception => error("get: " + e)
			    None
			}
		}


    def delete[T <: Data](kind: Kind[T], id: String): Boolean =
        {
        if (!checkConnect) return false
		val cp = new ColumnPath(kind.name)
		try
		    {
			conn.get.cli.remove(ByteBuffer.wrap(id.getBytes("UTF-8")), new ColumnPath(kind.name),
    			System.currentTimeMillis, ConsistencyLevel.ONE)
			kind.indices.foreach(i=>
			    {
				val name = kind.name + "_" + i.name
    			conn.get.cli.remove(ByteBuffer.wrap(id.getBytes("UTF-8")), new ColumnPath(name),
    			    System.currentTimeMillis, ConsistencyLevel.ONE)
				})
			true
			}
		catch
		    {
			case e:Exception => error("remove: " + e)
			    false
			}
        }
    
}

