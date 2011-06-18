


package pedro.data

import java.util.Date

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FeatureSpec,GivenWhenThen}



@RunWith(classOf[JUnitRunner])
class JsonTest extends FeatureSpec with GivenWhenThen
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
            assert(res.isDefined)
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
            assert(res)
            }

        scenario("JSON should be able to handle a series of escaped values")
            {
            given("A Json string with various escapes, one of them invalid")
            val str = """["AB\\CD","\x41BCD",324,23,true,"AB\u0043D"]"""
            when("The parser is called")
	        val res = JsonParser.parse(str)
            then("the parse should have failed")
            println("res:"+res)
	        assert(res.isEmpty)

            given("A Json string with various escapes, all ok")
            val str2 = """["AB\\CD",324,23,true,"AB\u0043D"]"""
            when("The parser is called")
            when("The parser is called")
	        val res2 = JsonParser.parse(str2)
            then("the parse should have succeeded")
            println("res2:"+res2)
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
    
    feature("Convert Product to JsonObject")
        {
        scenario("JSON should convert to and from Products correctly")
            {
            given("A Json object with various typed members")
            val date = new Date();
            val item = Item("hello", true, 1, 2L, 3.4, date)
            when("converted to Json")
	        val js = Json.toJson(item)
            then("conversion to a Json object should be successful")
            println("js:"+js.toString)
	        expect("hello")(js("sval").s)
	        expect(true)(js("bval").b)
	        expect(1)(js("ival").i)
	        expect(2L)(js("lval").l)
	        expect(3.4)(js("dval").d)
	        expect(date)(Json.parseDate(js("dateval")))
            }
        }

}


