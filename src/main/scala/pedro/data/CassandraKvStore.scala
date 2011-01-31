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
import org.apache.thrift.protocol.{TProtocol, TBinaryProtocol}
import org.apache.cassandra.service._
  
/**
 * A Key/Value store which uses a Cassandra database as a backend.
 * Note that the values to override are:  driver, url, user, pass.
 */
class CassandraKvStore(opts: Map[String, String] = Map())
    extends KvStore with pedro.util.Logged
{
    val host = opts.getOrElse("host", "127.0.0.1")
    val port = opts.getOrElse("port", "9160").toInt
    val user = opts.getOrElse("user", "")
    val pass = opts.getOrElse("pass", "")

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
     * Determines if an id already exists in a table
     */         
    def exists(name: String, id: String) : Boolean =
        {
        if (!checkConnect) return false
		false
        }
    
    /**
     * Puts a key/value pair into the table
     */         
    def put[T <: Data](kind: Kind[T], data: T) : Boolean =
        {
        if (!checkConnect) return false
		false
        }
    
    def get[T <: Data](kind: Kind[T], id: String): Option[T] =
        {
        if (!checkConnect) return None
		None
        }
    
    def list[T<:Data](kind: Kind[T]) : Option[Seq[T]] =
        {
        if (!checkConnect) return None
		None
        }


    def query[T<:Data, U<:Any](kind: Kind[T], index:Index[U], comp: (U)=>Boolean) : Option[Seq[T]] =
        {
        if (!checkConnect) return None
        None
		}


    def delete[T <: Data](kind: Kind[T], id: String): Boolean =
        {
        if (!checkConnect) return false
		false
        }
    
}

