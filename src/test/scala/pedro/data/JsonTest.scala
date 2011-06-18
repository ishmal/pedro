


package pedro.data

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.MustMatchers
import org.scalatest.{FeatureSpec,GivenWhenThen}



@RunWith(classOf[JUnitRunner])
class JsonTest extends FeatureSpec with GivenWhenThen with MustMatchers
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
            given("A Json string with various types")
            val str = simpleStr
            when("The parser is called")
            val res = JsonParser.parse(str)
            then("the parse should have been successful")
            res.isDefined must be === true
            println("### plain :\n" + res.get.toString)
            println("### pretty:\n" + res.get.pretty)
            }

        scenario("JSON should be able to parse a series of Json objects")
            {
            given("A Json string with multiple objects")
            val str = multiStr
            when("The parser is called")
            val res = JsonPush.parse(str)(js =>
	            {
	            println("## js: " + js.pretty)
	            })
            then("the parse should have been successful")
            res must be === true
            }

        scenario("JSON should be able to handle a series of escaped values")
            {
            given("A Json string with various escapes, one of them invalid")
            val str = """["AB\\CD","\x41BCD",324,23,true,"AB\u0043D"]"""
            when("The parser is called")
	        val res = JsonParser.parse(str)
            then("the parse should have failed")
            println("res:"+res)
	        res.isEmpty must be === true

            given("A Json string with various escapes, all ok")
            val str2 = """["AB\\CD",324,23,true,"AB\u0043D"]"""
            when("The parser is called")
            when("The parser is called")
	        val res2 = JsonParser.parse(str2)
            then("the parse should have succeeded")
            println("res2:"+res2)
	        res2.isDefined must be === true
            }
        }

}


