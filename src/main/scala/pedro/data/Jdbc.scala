/**
 * Simple JDBC wrapper utility
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



/**
 * Simple Scala JDBC utility.
 */
class Jdbc(conn: java.sql.Connection)  extends pedro.util.Logged
{
        
    /**
     * Use scalac 2.9.2's -Xexperimental flag if you want to refer
     * to the columns as result.column_name
     */
    class Result(override val self: Map[String, Any])
           extends scala.collection.MapProxy[String, Any] with Dynamic
        {
        def applyDynamic(name: String)(args: Any*) : Any =
            self(name)
        }

    def exec(sql: String) : Boolean =
        {
        try
            {
            val stmt = conn.createStatement
            val result = stmt.execute(sql)
            stmt.close
            result
            }
        catch
            {
            case e: Exception =>
                error("exec: " + e)
                false
            }
        }

    def query(sql: String) : Seq[Result] =
        {
        val xs = scala.collection.mutable.ListBuffer[Result]()
        try
            {
            val stmt = conn.createStatement
            val result = stmt.executeQuery(sql)
            val meta = result.getMetaData
            val nrColumns = meta.getColumnCount
            while (result.next)
                {
                val row = for (i <- 1 to nrColumns) yield
                    meta.getColumnName(i) -> result.getObject(i)
                xs += new Result(row.toMap.withDefaultValue(None))
                }
            stmt.close
            }
        catch
            {
            case e: Exception =>
                error("query: " + e)
            }
        xs.toSeq
        }

    def insert(table: String, values: Map[String, Any], where: String) : Boolean =
        {
        try
            {
            val buf = new StringBuilder
            buf.append("insert into ").append(table).append(" set ")
            buf.append(values.map(_._1 + "=?").mkString(","))
            if (where.size > 0)
                {
                buf.append(" where ")
                buf.append(where)
                }
            val stmt = conn.prepareStatement(buf.toString)
            var i = 1
            for (v <- values)
                {
                stmt.setObject(i, v._2)
                i += 1
                }
            val result = stmt.executeUpdate
            stmt.close
            true
            }
        catch
            {
            case e: Exception =>
                error("insert: " + e)
                false
            }
        }
         
    def update(table: String, values: Map[String, Any], where: String) : Boolean =
        {
        try
            {
            val buf = new StringBuilder
            buf.append("update ").append(table).append(" set ")
            buf.append(values.map(_._1 + "=?").mkString(","))
            if (where.size > 0)
                {
                buf.append(" where ")
                buf.append(where)
                }
            val stmt = conn.prepareStatement(buf.toString)
            var i = 1
            for (v <- values)
                {
                stmt.setObject(i, v._2)
                i += 1
                }
            val result = stmt.executeUpdate
            stmt.close
            true
            }
        catch
            {
            case e: Exception =>
                error("update: " + e)
                false
            }
        }
         
    def delete(table: String, where: String) : Boolean =
        {
        try
            {
            val buf = new StringBuilder
            buf.append("delete from ").append(table)
            if (where.size > 0)
                {
                buf.append(" where ")
                buf.append(where)
                }
            val sql = buf.toString
            val stmt = conn.createStatement
            val result = stmt.execute(sql)
            stmt.close
            result
            }
        catch
            {
            case e: Exception =>
                error("delete: " + e)
                false
            }
        }

    def close =
        {
        try
            {
            conn.close
            }
        catch
            {
            case e: Exception =>
                error("close: " + e)
            }
        }
}




object Jdbc extends pedro.util.Logged
{
    def apply(jdbcDriver: String, jdbcUrl: String, user: String, pass: String) : Option[Jdbc] =
        {
        try
            {
            java.lang.Class.forName(jdbcDriver).newInstance
            val conn = java.sql.DriverManager.getConnection(jdbcUrl, user, pass)
            Some(new Jdbc(conn))
            }
        catch
            {
            case e: Exception =>
                error("apply: " + e)
                None
            }
        }
}



