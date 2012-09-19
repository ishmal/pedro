/**
 * Simple JSON classes and parser
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (C) 2010-2012 Bob Jamison
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

import language.dynamics

import java.util.Date

/**
 * Common trait for all things that want to output Json strings.
 * Users need not be members of this package.
 */ 
trait JsonOutput
{
    private val hex = "0123456789abcdef".toCharArray

    /**
     * Output a JSON string with all of the proper escapes
     */       
    def jsonStr(ins: String) : String =
        {
        val buf = new StringBuilder
        buf.append('"')
        ins.foreach(ch=>
            {
            if (ch == '\\')       buf.append("\\\\")
            else if (ch == '\f')  buf.append("\\f")
            else if (ch == '\b')  buf.append("\\b")
            else if (ch == '\t')  buf.append("\\t")
            else if (ch == '\r')  buf.append("\\r")
            else if (ch == '\n')  buf.append("\\n")
            else if (ch >= 32 && ch < 127) buf.append(ch)
            else buf.append("\\u").
                 append(hex((ch >> 12)&0xf)).append(hex((ch >>  8)&0xf)).
                 append(hex((ch >>  4)&0xf)).append(hex((ch      )&0xf))
            })
        buf.append('"')
        buf.toString
        }
    
}



/**
 * Common trait for all of the different JsonValue types.  Has common serialization
 * code and methods that should be visible for any JsonValue.  The purpose is to
 * allow the user to use any of these methods on any JsonValue, much like those
 * metods and properties would be available in Javascript.  
 */  
trait JsonValue extends JsonOutput
{

    /**
     * The basic string representation of this JsonValue.  Override this for each type
     */  
    override def toString : String =
        {
        "[this trait not implemented: " + getClass.getName + "]"
        }

    /**
     * "Pretty-print" this JsonValue.  Override this for each type
     */  
    def pretty(indent: Int) : String = 
        {
        toString
        }

    /**
     * "Pretty-print" this JsonValue.  Do not override.
     */  
    def pretty : String = 
        {
        pretty(0)
        }
    
    
    /**
     * Every JsonValue type will have an apply(key) and apply(index) method,
     * so that we may chain apply()'s together as a path, like:
     * {
     *  "hello" :
     *      {
     *      "world" : [9,8,7,6,5,4]     
     *      }          
     * }     
     *    jval("hello")("world")(3)
     * ...should return a JsonInt(6)               
     * We will provide no-val returns for inappropriate types, so the user
     * need not worry about exceptions.     
     */              
    
    /**
     * Return a named property of a JsonObject.  Classes other than JsonObject
     *      return JsonNil
     * Usage:   val jv = jval("fieldName")    
     */
    def apply(name: String) : JsonValue =
        JsonNil

    /**
     * Return an indexed member of a JsonArray.  Classes other than JsonArray
     * will return JsonNil.  An index out of bounds will also return JsonNil.        
     */
    def apply(indx: Int) : JsonValue =
        JsonNil

    /**
     * Return the internal Map implementation of a JsonObject.  Classes other
     * than JsonObject will return an empty map.  This is useful for walking an
     * object tree
     */              
    def toMap : Map[String, JsonValue] =
        Map()

    /**
     * Convenience method.  Iterate over the children of a JsonArray, applying
     * f() to each one.  Classes other than JsonArray will do nothing.
     */              
    def foreach(f: (JsonValue) => Unit): Unit =
        {}

    /**
     * Convenience method.  Map each object of a JsonArray to type U, returning
     * a Seq of type U.  Classes other than JsonArray will return an empty
     * Seq.     
     */              
    def map[U](f: (JsonValue) =>  U): Seq[U] =
        List()

    /**
     * Return a List of the members of a JsonArray, to allow the use of List's API
     * on the members of the List.  Classes other than JsonArray will return an empty
     * List.     
     */              
    def toSeq : Seq[JsonValue] =
        List()

    /**
     * Return the double value of a JsonDouble, else try to convert.
     * Notice that we will accept the member of a single-member array.     
     */              
    def d : Double =
        this match
            {
            case JsonInt(v)     => v.toDouble
            case JsonBoolean(v) => if (v) 1.0 else 0.0
            case JsonDouble(v)  => v
            case JsonArray(v)   => if (v.size==1) v(0).d else 0.0
            case JsonString(v)  => try
                                       { v.trim.toDouble }
                                   catch
                                       { case e:Exception => 0.0 }
            case _              => 0.0
            }

    /**
     * Return the long value of a JsonInt, else try to convert
     */              
    def i : Int =
        this match
            {
            case JsonInt(v)     => v.toInt
            case JsonBoolean(v) => if (v) 1 else 0
            case JsonDouble(v)  => v.toInt
            case JsonArray(v)   => if (v.size==1) v(0).i else 0
            case JsonString(v)  => try
                                       { v.trim.toInt }
                                   catch
                                       { case e:Exception => 0 }
            case _              => 0
            }

    /**
     * Return the long value of a JsonInt, else try to convert
     */              
    def l : Long =
        this match
            {
            case JsonInt(v)     => v
            case JsonBoolean(v) => if (v) 1 else 0
            case JsonDouble(v)  => v.toLong
            case JsonArray(v)   => if (v.size==1) v(0).i else 0
            case JsonString(v)  => try
                                       { v.trim.toLong }
                                   catch
                                       { case e:Exception => 0L }
            case _              => 0
            }

    /**
     * Return the string value of a JsonString, else try to convert
     */              
    def s : String =
        this match
            {
            case JsonString(v)  => v
            case JsonArray(v)   => if (v.size==1) v(0).s else ""
            case JsonInt(v)     => v.toString
            case JsonDouble(v)  => v.toString
            case JsonBoolean(v) => v.toString
            case _              => ""
            }

    /**
     * Return the boolean value of a JsonBoolean, else try to convert
     */              
    def b : Boolean =
        this match
            {
            case JsonBoolean(v) => v
            case JsonInt(v)     => (v != 0)
            case JsonDouble(v)  => (v != 0.0)
            case JsonArray(v)   => if (v.size==1) v(0).b else false
            case JsonString(v)  => v.trim.equalsIgnoreCase("true")
            case _              => false
            }
}


/**
 * Some very useful implicits that should be available for any JsonValue
 */ 
object JsonValue
{
    implicit def json2boolean(j:JsonValue)    : Boolean      = j.b
    implicit def json2booleanSeq(j:JsonValue) : Seq[Boolean] = j.map(_.b)
    implicit def json2double(j:JsonValue)     : Double       = j.d
    implicit def json2doubleSeq(j:JsonValue)  : Seq[Double]  = j.map(_.d)
    implicit def json2int(j:JsonValue)        : Int          = j.i.toInt
    implicit def json2intSeq(j:JsonValue)     : Seq[Int]     = j.map(_.i.toInt)
    implicit def json2long(j:JsonValue)       : Long         = j.l
    implicit def json2longSeq(j:JsonValue)    : Seq[Long]    = j.map(_.l)
    implicit def json2string(j:JsonValue)     : String       = j.s
    implicit def json2stringSeq(j:JsonValue)  : Seq[String]  = j.map(_.s)
    implicit def json2date(j:JsonValue)       : Date         = Json.parseDate(j)
    implicit def json2dateSeq(j:JsonValue)    : Seq[Date]    = j.map(v=>Json.parseDate(v))
}



/**
 * The JsonObject class.  In text this is formatted:
 * {
 *   "name0" : value0,
 *      ... 
 *   "nameN" : valueN 
 * }  
 */
case class JsonObject(value: Map[String,JsonValue]) extends JsonValue
{
    override def toString : String =
        {
        val sorted = value.toList.sortWith((a,b) => a._1<b._1)
        val vals = sorted.map(a => jsonStr(a._1) + ":" + a._2.toString)
        vals.mkString("{", ",", "}")
        }

    override def pretty(indent: Int) : String =
        {
        val sorted = value.toList.sortWith((a,b) => a._1<b._1)
        val maxkey = sorted.foldLeft(0)(_ max _._1.length)
        val buf = new StringBuilder
        val startln = "\n" + (" "*indent)
        val openBrace  = startln + "{"
        val closeBrace = startln + "}"
        val pretties = sorted.map(a=> startln + jsonStr(a._1) +
               (" "*(maxkey-a._1.length)) + " : " + a._2.pretty(indent+4))
        buf.append(pretties.mkString(openBrace, ",", closeBrace))
        buf.toString
        }
    
    /**
     * Map-like methods from JsonValue
     */         
    override def apply(name: String) : JsonValue =
        value.getOrElse(name, JsonNil)

    override def toMap : Map[String, JsonValue] =
        value

}


/**
 * The JsonArray class.  In text this is formatted:
 * [ value0, value1, ... valueN ]
 */
case class JsonArray(value: Seq[JsonValue]) extends JsonValue
{
    override def toString : String = 
        {
        value.map(_.toString).mkString("[", ",", "]")
        }

    override def pretty(indent: Int) =
        {
        val buf = new StringBuilder
        val startln = "\n" + (" "*indent)
        val openBracket  = startln + "["
        val closeBracket = startln + "]"
        val pretties = value.map(e=> startln + e.pretty(indent+4))
        buf.append(pretties.mkString(openBracket, ",", closeBracket))
        buf.toString
        }

    /**
     * Seq-like methods from JsonValue
     */         
    override def apply(indx: Int) : JsonValue =
        try
            { value(indx) }
        catch
            { case e: IndexOutOfBoundsException => JsonNil }

    override def foreach(f: (JsonValue) => Unit): Unit =
        value.foreach(f)

    override def map[U](f: (JsonValue) =>  U): Seq[U] =
        value.map(f)

    override def toSeq : Seq[JsonValue] =
        value.toSeq
}



/**
 * Primitives
 */ 
case object JsonNil extends JsonValue
{
    override def toString = "null"
}
case class JsonString(value: String) extends JsonValue
{
    override def toString = "\"" + value + "\""
}
case class JsonDouble(value: Double) extends JsonValue
{
    override def toString = value.toString
}
case class JsonInt(value: Long) extends JsonValue
{
    override def toString = value.toString
}
case class JsonBoolean(value: Boolean) extends JsonValue
{
    override def toString = value.toString
}




/**
 * 
 */ 
object Json
{
    import scala.reflect.ClassTag
    import scala.reflect.runtime.universe._
    val mirror = scala.reflect.runtime.currentMirror
    
    val DATE_FORMAT = "yyyyMMddHHmmss.SSSZ"
      
    lazy val dateFormatter = new java.text.SimpleDateFormat(DATE_FORMAT)
    
    def parseDate(v: JsonValue) : Date =
        dateFormatter.parse(v)

    /**
     * Generate a JsonValue object tree representing an object.
     */                   
    def toJson(obj: Any) : JsonValue =
        {
        def fromInstance(obj: Any) : JsonValue = obj match
            {
            case v: JsonValue   => v
            case v: String      => JsonString(v)
            case v: Double      => JsonDouble(v)
            case v: Float       => JsonDouble(v)
            case v: Int         => JsonInt(v)
            case v: Long        => JsonInt(v)
            case v: Boolean     => JsonBoolean(v)
            case v: Date        => JsonString(dateFormatter.format(v))
            case v: Iterable[_] => toArray(v)
            case v: Array[_]    => toArray(v)
            case v: Any         => toObject(v)
            case _              => JsonNil  // error?
            }

        def toArray(arr: Iterable[_]) : JsonArray =
            new JsonArray(arr.map(v=>fromInstance(v)).toList)

        //After 2 years, finally we get scala reflection.  YAAY!
        def toObject[T : ClassTag](obj: T) : JsonObject =
            {
            val im = mirror.reflect(obj)
            val syms = im.symbol.typeSignature.members.collect
                  {
                  case m: MethodSymbol if m.isAccessor => m
                  }.toList
            val props = for (sym <- syms) yield
                {
                val name = sym.name.toString
                val value = im.reflectMethod(sym).apply()
                (name, fromInstance(value))
                }
            val classTuple = Seq(("_class_" , JsonString(obj.getClass.getName)))
            new JsonObject((props ++ classTuple).toMap)
            }

        fromInstance(obj)
        }
    
    def toString(obj: Any) =
        toJson(obj).toString

    private val doubleMatcher = "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?".r

    //Convert a short string to a primitive
    def primitive(str: String) : JsonValue =
        {
        if (str == "true")
            JsonBoolean(true)
        else if (str == "false")
            JsonBoolean(false)
        else if (str == "null")
            JsonNil
        else if (doubleMatcher.pattern.matcher(str).matches)
            try  //try int first, then double
                { JsonInt(str.toLong) }
            catch
                { case _ : Throwable => JsonDouble(str.toDouble) }
        else 
            JsonString(str)
        }
    
    val registeredFactories = 
        scala.collection.mutable.Map[String,(JsonValue)=>Any]()
    
    /**
     * If you want an object serialized by this tool to be automatically reparseable
     * into an object, then register a function (possibly during your class's constructor)
     * Example:
     * <pre>
     *  case class Item(
     *    val sval    : String = "",
     *    val bval    : Boolean = false,
     *    val ival    : Int = 0,
     *    val lval    : Long = 0L,
     *    val dval    : Double = 0.0,
     *    val dateval : Date = new Date
     *  )
     *  {
     *  Json.registerFactory(this, {js=>
     *      Item(
     *          sval    = js("sval"),
     *          bval    = js("bval"),
     *          ival    = js("ival"),
     *          lval    = js("lval"),
     *          dval    = js("dval"),
     *          dateval = js("dateval")
     *          )
     *      })
     *  }
     *
     * </pre>
     * 
     * Once all desired classes have been registered, then val obj = Json.deserialize(js)
     * should return a valid Any.
     */
    def registerFactory(clazz: Class[_], factory: (JsonValue) => Any) =
        {
        registeredFactories += clazz.getName -> factory
        }

    /**
     * Creates a object of a class that has been registered here with registerFactory
     *      
     * @param js a JsonValue to use to initialize an object of the given class. 
     *      This should be a JsonObject with a "_class_" property that with the same
     *      class name that was used in the call to registerFactory()               
     * @return an object of the registered type specified by its "_class_" property,
     * or None if not found.  Pattern matching can be used to collect the results.          
     */
    def deserialize(js: JsonValue) : Any =
        {
        val factory = registeredFactories.get(js("_class_"))
        if (factory.isEmpty)
            None
        else
            factory.get(js)
        }

}



/**
 * A simple parser for reading JSON data as defined at http://json.org .
 */ 
class JsonParser(debug: Boolean = false) extends pedro.util.Logged
{
    /**
     * Overrideable to allow message redirection, such as log.trace()
     */         
    override def trace(str: String) =
        if (debug) super.trace(str)

    /**
     * Overrideable to allow message redirection, such as log.error()
     */         
    def error(pos: Int, str: String) : Unit =
        error("JsonParser error (" + pos + "): " + str)

    private val EOF = (-1).asInstanceOf[Char]

    private var parsebuf = ""
    
    private val doubleMatcher = "[-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?".r
    private val hex4Matcher    = "[0-9a-fA-F]{4}".r 
    
    private def get(pos: Int) = 
        try
            { parsebuf(pos) }
        catch
            { case e: IndexOutOfBoundsException => EOF }
    
    private def skipwhite(pos: Int) : Int =
        {
        var p = pos
        while (p < parsebuf.length && parsebuf(p).isWhitespace)
            p += 1
        p
        }

    private def matchStr(pos: Int, str:String) = 
        parsebuf.regionMatches(pos, str, 0, str.size)
        
    private def matchReg(pos: Int, regex:scala.util.matching.Regex) = 
        regex.findPrefixOf(parsebuf.substring(pos))
        
    private def quotedString(pos: Int, quoteChar: Char) : (String, Int) =
        {
        trace("quotedString")
        val buf = new StringBuilder
        var p = pos
        while (p < parsebuf.size)
            {
            val ch = get(p)
            p += 1
            ch match
                { //escapes.  see http://json.org for string escape sequences
                case '\\' =>
                    {
                    val ch2 = get(p)
                    p += 1
                    ch2 match 
                        { 
                        case '\\' => buf += '\\'
                        case '/'  => buf += '/'
                        case '"'  => buf += '"'
                        case 'f'  => buf += '\f'
                        case 'b'  => buf += '\b'
                        case 't'  => buf += '\t'
                        case 'r'  => buf += '\r'
                        case 'n'  => buf += '\n'
                        case 'u'  =>
                            {
                            val res = matchReg(p, hex4Matcher)
                            if (res.isDefined)
                                {
                                buf += Integer.parseInt(res.get,16).asInstanceOf[Char]
                                p += 4
                                }
                            else
                                {
                                error(p, "bad unicode value in string")
                                return ("", -1)
                                }
                            }
                        case _ =>
                            {
                            error(p, "unknown escape in string: '" + ch2 + "'")
                            return ("", -1)
                            }
                        } // ch2
                    }
                case _   => if (ch == quoteChar)
                                 return (buf.toString, p)
                            else
                                buf += ch
                }//ch
            }
        error(p, "unterminated string")
        ("", -1)
        }
    
    //This is  quotedString ~ ":" ~ jsonValue
    private def jsonEntry(pos: Int) : (String, JsonValue, Int) =
        {
        trace("jsonEntry")
        var p = skipwhite(pos)
        val ch = get(p)
        if (ch != '"' && ch != '\'')
            {
            error(p, "expected property name string")
            return ("", JsonNil, -1)
            }
        else
            {
            val (name, qpos) = quotedString(p+1, ch)
            if (qpos < 0)
                return ("", JsonNil, -1)
            else
                {
                p = skipwhite(qpos)
                val ch2 = get(p)
                if (ch2 != ':')
                    {
                    error(p, "expected ':' in object property")
                    return ("", JsonNil, -1)
                    }
                else
                    {
                    p = skipwhite(p+1)
                    val (jval, jpos) = jsonValue(p)
                    if (jpos < 0)
                        return ("", JsonNil, -1)
                    else
                        {
                        p = skipwhite(jpos)
                        return (name, jval, p)
                        }
                    }
                }
            }
        }

    //this is  "{" ~ rep(jsonEntry, ",") ~ "}" 
    private def jsonObject(pos: Int) : (JsonValue, Int) =
        {
        trace("jsonObject")
        var props = Map[String,JsonValue]()
        var p = skipwhite(pos)
        var first = true
        while (p < parsebuf.size)
            {
            val ch = get(p)
            if (ch == '}')
                return (JsonObject(props), p+1)
            else
                {
                if (!first)
                    {
                    if (ch != ',')
                        {
                        error(p, "expected ',' between object properties")
                        return (JsonNil, -1)
                        }
                    else
                        p += 1
                    }
                val (name, jval, epos) = jsonEntry(p)
                if (epos < 0)
                    return (JsonNil, -1)
                else
                    {
                    props += name -> jval
                    p = skipwhite(epos)
                    first = false
                    }
                }
            }
        error(p, "unterminated object")
        (JsonNil, -1)
        }

    //this is  "[" ~ rep(JsonValue, ",") ~ "]"
    private def jsonArray(pos: Int) : (JsonValue, Int) =
        {
        trace("jsonArray")
        val xs = scala.collection.mutable.ListBuffer[JsonValue]()
        var p = skipwhite(pos)
        var first = true
        while (p < parsebuf.size)
            {
            val ch = get(p)
            if (ch == ']')
                return (JsonArray(xs.toList), p+1)
            else
                {
                if (!first)
                    {
                    if (ch != ',')
                        {
                        error(p, "expected ','")
                        return (JsonNil, -1)
                        }
                    else
                        p += 1
                    }
                val (jval, jpos) = jsonValue(p)
                if (jpos < 0)
                    return (JsonNil, -1)
                else
                    {
                    xs += jval
                    p = skipwhite(jpos)
                    first = false
                    }
                }
            }
        error(p, "unterminated array")
        (JsonNil, -1)
        }

    private def jsonString(pos: Int, quoteChar: Char) : (JsonValue, Int) =
        {
        trace("jsonString")
        val (str, spos) = quotedString(pos, quoteChar)
        if (spos < 0)
            (JsonNil, -1)
        else
            (JsonString(str), spos)
        }

    private def jsonValue(pos: Int) : (JsonValue, Int) =
        {
        trace("jsonValue")
        var p = skipwhite(pos)
        if (p < 0)
            (JsonNil, -1)
        else
            {
            val ch = get(p)
            if (ch == '{') 
                jsonObject(p+1)
            else if (ch == '[') 
                jsonArray(p+1)
            else if (ch == '"' || ch == '\'')
                jsonString(p+1, ch)
            else if (matchStr(p, "true"))
                (JsonBoolean(true), p+4)
            else if (matchStr(p, "false"))
                (JsonBoolean(false), p+5)
            else if (matchStr(p, "null"))
                (JsonNil, p+4)
            else 
                {
                val res = matchReg(p, doubleMatcher)
                if (res.isDefined)
                    try  //try int first, then double
                        { (JsonInt(res.get.toLong), p+res.get.size) }
                    catch
                        { case _ : Throwable => (JsonDouble(res.get.toDouble), p+res.get.size) }
                else 
                    {
                    error(p, "unknown syntax for json value: '" + ch + "'")
                    (JsonNil, -1)
                    }
                }
            }
        }


    /**
     * Parse a string to a JsonValue.
     * Example:
     * val jval = (new JsonParser).parse("{ 'hello_world' : 12345 }")
     * if (jval.isDefined)
     *     println(jval.get)                 
     */         
    def parse(str: String = "") : Option[JsonValue] =
        {
        trace("parse")
        parsebuf = str
        val (jval, pos) = jsonValue(0)
        if (pos<0)
            None
        else
            {
            val p = skipwhite(pos)
            if (p < str.size-1)
                {
                error(p, "extra characters at end of parsed stream")
                None
                }
            else
                Some(jval)
            }
        }

}



/**
 * A simple parser for reading JSON data as defined at http://json.org .
 */ 
object JsonParser extends pedro.util.Logged
{
    /**
     * Parse a string to a JsonValue.
     * Example:
     * val jval = JsonParser.parse("{ 'hello_world' : 12345 }")
     * if (jval.isDefined)
     *     println(jval.get)                 
     */
    def parse(str: String = "", debug: Boolean = false) : Option[JsonValue] =
        (new JsonParser(debug)).parse(str)

    /**
     * Parse a URL to a JsonValue.  This is a convenience method for retrieving
     * remote API results.     
     * Example:
     * val jval = JsonParser.parse("http://some-server/path/to/value")
     * if (jval.isDefined)
     *     println(jval.get)                 
     */
    def parseURL(urls: String = "", debug: Boolean = false) : Option[JsonValue] =
        {
        try
            {
            val str = scala.io.Source.fromURL(new java.net.URL(urls)).mkString
            if (debug)
                {
                trace("######################### From :" + urls)
                trace(str)
                trace("#########################")
                }
            parse(str, debug)
            }
        catch
            {
            case e: Exception => error("JsonParser.parseURL: " + e)
            None
            }
        }

   /**
     * Parse a file to a JsonValue.  Convenience method.
     */
    def parseFile(fname: String = "", debug: Boolean = false) : Option[JsonValue] =
        {
        try
            {
            val str = scala.io.Source.fromFile(fname)("UTF-8").mkString
            if (debug)
                {
                trace("######################### From :" + fname)
                trace(str)
                trace("#########################")
                }
            parse(str, debug)
            }
        catch
            {
            case e: Exception => error("JsonParser.parseFile: " + e)
            None
            }
        }

}




/**
 * This is a simple "push" front end that allows character-at-a-time processing
 * of output streams without requiring otherwise unnecessary pipes which can
 * slow down code.  This state machine tracks the nesting level of "{" open braces.
 * A { increments and a } decrements the level.  When a } causes the depth to hit
 * a low threshold, the buffer will be parsed and cleared, and the state machine
 * reset.  If the parser successfully parses a JsonObject, then it is returned.
 * If a function of f(js:JsonObject) => Unit is passed to the constructor, then
 * that function is called.
 */       
class JsonPush(
    val threshold: Int = 0,
    val f: (JsonObject)=>Unit = _ => (),
    val debug: Boolean = false
    ) extends pedro.util.Logged
{
    var depth   = 0       // how nested in tags?
    var inQuote = false   // we are in " ... "
    var escaped = false   // is it possible that " is escaped?
    
    val buf = new StringBuilder

    private def reset
        {
        buf.clear
        depth     = 0
        inQuote   = false
        escaped   = false
        }
    
    def out(str: String) : Option[JsonValue] =
        {
        trace("out: " + str)
        val res = JsonParser.parse(str)
        res match
            {
            case v @ Some(obj: JsonObject) => f(obj) ; v
            case _ => error("Not a JsonObject") ; None
            }
        }


    /**
     *  This is the state machine.  Please understand this
     *  before modifying.     
     */
    def +(chr: Int) : Option[JsonValue] =
        {
        val ch = chr.asInstanceOf[Char]
        //trace("ch : " + ch)
        buf.append(ch)
        if (inQuote)
            {
            if (ch == '"' && !escaped)
                inQuote = false
            }
        else
            {
            if (ch == '{')
                depth += 1
            else if (ch == '}')
                {
                depth -= 1
                if (depth <= threshold)
                    {
                    val res = out(buf.toString)
                    reset
                    return res
                    }
                }
            if (ch == '"' && !escaped)
                inQuote = true
            else if (ch == '\\')
                escaped = true
            else
                escaped = false
            }
        Some(JsonNil)
        }
}



/**
 * Convenience object for calling the push parser
 */ 
object JsonPush
{
    def parse(str: String = "", depth:Int = 0)(f: (JsonValue)=>Unit) : Boolean =
        {
        val parser = new JsonPush(depth, f)
        //the following means, no character passed to the parser can return a None
        !str.exists{ch => (parser + ch) == None}
        }

    def parseFile(fname: String, depth:Int = 0)(f: (JsonValue)=>Unit) : Boolean =
        {
        val parser = new JsonPush(depth, f)
        //the following means, no character passed to the parser can return a None
        !scala.io.Source.fromFile(fname).exists{ch => (parser + ch) == None}
        }
}




