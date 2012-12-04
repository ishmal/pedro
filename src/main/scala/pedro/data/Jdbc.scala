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
    var lastError = ""
    
    override def error(str: String) =
        {
        super.error(str)
        lastError = str
        }
        
    /**
     * Use scalac 2.9.2's -Xexperimental flag if you want to refer
     * to the columns as result.column_name
     * 
     * result("field") will return the value with that column name.  If
     * the column does not exist, return None.
     * 
     * Note: The default None of the Map is created during the query                         
     */
    class Result(override val self: Map[String, Any])
           extends scala.collection.MapProxy[String, Any] with Dynamic
        {
        def applyDynamic(name: String)(args: Any*) : Any =
            self(name)
        }

    def exec(sql: String) : Boolean =
        {
        val stmt = conn.createStatement
        try
            {
            val result = stmt.execute(sql)
            stmt.close
            result
            }
        catch
            {
            case e: Exception =>
                error("exec: " + e)
                stmt.close
                false
            }
        }

    /**
     * Query a table with the given SQL string.  
     * 
     * @param sql the SQL string to execute.          
     * @return Some(Seq[Result]) if successful, None on failure
     * 
     * Typical usage:
     *     val res = jdbc.query("select * from table_name where city='chicago'")
     *     if (res.isDefined)
     *         {
     *         for (row <- res.get)
     *              {
     *              println("last_name" + row("last_name"))  
     *              //  or..    row.last_name    ..if you use Dynamic                 
     *              }                         
     *         }
     *      else
     *         {
     *         println("there was a problem: " + jdbc.lastError)    
     *         }               
     */           
    def query(sql: String) : Option[Seq[Result]] =
        {
        val stmt = conn.createStatement
        try
            {
            val xs = scala.collection.mutable.ListBuffer[Result]()
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
            Some(xs.toSeq)
            }
        catch
            {
            case e: Exception =>
                error("query: " + e)
                stmt.close
                None
            }
        }


    /**
     * Inserts a new row into a table
     *      
     * @param table the name of the table
     * @param values a Map of column name -> value for the columns that
     *     will be inserted
     * @return true if successful, else false
     */                          
    def insert(table: String, values: Map[String, Any]) : Boolean =
        {
        var stmt : Option[java.sql.PreparedStatement] = None
        try
            {
            val buf = new StringBuilder
            buf.append("insert into ").append(table).append(" ")
            buf.append(values.map(_._1).mkString("(", ",", ")"))
            buf.append(" values ")
            buf.append(values.map(_ => "?").mkString("(", ",", ")"))
            stmt = Some(conn.prepareStatement(buf.toString))
            var i = 1
            for (v <- values)
                {
                stmt.get.setObject(i, v._2)
                i += 1
                }
            stmt.get.executeUpdate
            stmt.get.close
            true
            }
        catch
            {
            case e: Exception =>
                error("insert: " + e)
                if (stmt.isDefined) error("the statement is:" + stmt.get)
                stmt.foreach(_.close)
                false
            }
        }
         
    /**
     * Updates one or more rows in a table
     *      
     * @param table the name of the table
     * @param values a Map of column name -> value for the columns that
     *     will be inserted
     * @param where.  The SQL where clause that selects which
     *     rows to update.  Can be "" to update all records.
     * @return >= 0 for the number of rows updated if successful.  -1 on error.
     */                          
    def update(table: String, values: Map[String, Any], where: String) : Int =
        {
        var stmt : Option[java.sql.PreparedStatement] = None
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
            stmt = Some(conn.prepareStatement(buf.toString))
            var i = 1
            for (v <- values)
                {
                stmt.get.setObject(i, v._2)
                i += 1
                }
            val result = stmt.get.executeUpdate
            stmt.get.close
            result
            }
        catch
            {
            case e: Exception =>
                error("update: " + e)
                stmt.foreach(_.close)
                -1
            }
        }
         
    /**
     * Delete records from a table
     * @param table the name of the table
     * @param where.  The SQL where clause that selects which
     *     rows to delete.  Can be "" to delete all records.
     * @return true on success, else false
     */                        
    def delete(table: String, where: String) : Boolean =
        {
        val stmt = conn.createStatement
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
            val result = stmt.execute(sql)
            stmt.close
            result
            }
        catch
            {
            case e: Exception =>
                error("delete: " + e)
                stmt.close
                false
            }
        }

    /**
     * Closes this JDBC connection.  Closing an already-closed
     * connection should have no effect.
     *      
     * @return true on success, else false     
     */              
    def close : Boolean =
        {
        try
            {
            conn.close
            true
            }
        catch
            {
            case e: Exception =>
                error("close: " + e)
                false
            }
        }
}



/**
 * Jdbc companion object & utility
 */ 
object Jdbc extends pedro.util.Logged
{
    /**
     * Simple Jdbc connection constructor.
     * 
     * Typical usage:
     *     val jdbcDriver = "com.mysql.jdbc.Driver"
     *     val jdbcUrl    = "jdbc:mysql://someserver/database"
     *     val user       = "user"
     *     val pass       = "pass"
     *     val res = Jdbc(jdbcDriver, jdbcUrl, user, pass)
     *     
     *     if (res.isEmpty)
     *         {
     *         //report an error      
     *         }                
     *     else
     *         {
     *         jdbc = res.get
     *         // do your sql stuff          
     *         jdbc.close
     *         }                        
     */         
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



