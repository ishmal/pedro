/**
 * Simple JDBC wrapper utility
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (C) 2012 Bob Jamison
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

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FeatureSpec}
import org.scalatest.matchers.{ShouldMatchers}

@RunWith(classOf[JUnitRunner])
class JdbcTest extends FeatureSpec with ShouldMatchers
{
    val jdbcDriver = "com.mysql.jdbc.Driver"
    val jdbcUrl    = "jdbc:mysql://someserver/database"
    val user       = "user"
    val pass       = "pass"
    val sql        = "select * from table"
    
    def doTest : Boolean =
        {
        val ret = Jdbc(jdbcDriver, jdbcUrl, user, pass)
        if (!ret.isDefined)
            {
            info("Connection failed")
            return false
            }
        val jdbc = ret.get
        val result = jdbc.query(sql)
        for (row <- result)
            {
            println(row)
            // println(row("column_name"))
            // println(row.column_name) //see Dynamic above
            }
        jdbc.close
        true
        }

}

