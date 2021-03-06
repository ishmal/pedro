/**
 * Test JSON capabilities
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

import java.util.Date

@RunWith(classOf[JUnitRunner])
class JsonTest extends FeatureSpec with ShouldMatchers
{
    val simpleStr = 
"""
{
    "string" : "hello, world, \uabcd",
    "double" : 1.234,
    "boolean" : true,
    "integer" : 12345,
    "nil"     : null,
    "array"   :
        [
        "hello, world",
        1.234,
        true,
        12345,
        null
        ],
    "object"  :
        {
        "string" : "hello, world",
        "double" : 1.234,
        "boolean" : true,
        "integer" : 12345,
        "nil"     : null
        }
}
"""

    val multiStr = 
"""
{
    "object1"  :
        {
        "string" : "hello, world",
        "double" : 1.234,
        "boolean" : true,
        "integer" : 12345,
        "nil"     : null
        }
}
{
    "object2"  :
        {
        "string" : "hello, world",
        "double" : 1.234,
        "boolean" : true,
        "integer" : 12345,
        "nil"     : null
        }
}
"""

    feature("JSON serialization and deserialization")
        {
        scenario("JSON should be able to parse a simple Json object")
            {
            val str = simpleStr
            val res = JsonParser.parse(str)
            res.isDefined should be === true
            info("### plain :\n" + res.get.toString)
            info("### pretty:\n" + res.get.pretty)
            }

        scenario("JSON should be able to parse a series of Json objects")
            {
            val str = multiStr
            val res = JsonPush.parse(str)(js =>
	            {
	            info("## js: " + js.pretty)
	            })
            assert(res)
            }

        scenario("JSON should be able to handle a series of escaped values")
            {
            val str = """["AB\\CD","\xabcd", 324,23,true,"AB\u0043D"]"""
	        val res = JsonParser.parse(str)
            info("res:"+res)
	        assert(res.isEmpty)

            val str2 = """["AB\\CD",324,23,true,"AB\u0043D"]"""
	        val res2 = JsonParser.parse(str2)
            info("res2:"+res2)
	        assert(res2.isDefined)
            }
        }

    case class Item(
        val sval    : String = "",
        val bval    : Boolean = false,
        val ival    : Int = 0,
        val lval    : Long = 0L,
        val dval    : Double = 0.0,
        val dateval : Date = new Date
        )
        {
        Json.registerFactory(getClass, {js=>
            Item(
                sval    = js("sval"),
                bval    = js("bval"),
                ival    = js("ival"),
                lval    = js("lval"),
                dval    = js("dval"),
                dateval = js("dateval")
                )
            })
        }
    
    feature("Convert Product to JsonObject")
        {
        scenario("JSON should convert to and from objects correctly")
            {
            val date = new Date();
            val item = Item("hello", true, 1, 2L, 3.4, date)
	        val js = Json.toJson(item)
            val jss : String = js.pretty
            info("js:" + jss)
	        expectResult("hello")(js("sval").s)
	        expectResult(true)(js("bval").b)
	        expectResult(1)(js("ival").i)
	        expectResult(2L)(js("lval").l)
	        expectResult(3.4)(js("dval").d)
	        expectResult(date)(Json.parseDate(js("dateval")))
	        
	        val res = Json.deserialize(js)
	        val isCorrectType = res match
	            {
                case v:Item => true
                case _ => false
	            }
            assert(isCorrectType)
            }
        }

}


