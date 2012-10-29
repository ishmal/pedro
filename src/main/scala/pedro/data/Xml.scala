/**
 * Pedro XMPP Client
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


import scala.collection.mutable.ListBuffer

/**
 * The purpose of this file is to provide a small data-oriented parser for
 * applications. 
 * 
 * This library is unsuitable for document processing.  Users should seek
 * a larger, more robust library for that.
 */      
trait Node
{
    val namespace : String
    val prefix    : String
    val name      : String
    val value     : String

    def apply(attrName: String) : String =
        ""

    def \(path: String) : XpathResult =
        new XpathResult(Nil)

    def \=(path: String) : String =
        (this \ path).text

    def \#(path: String) : Int =
        {
        try
            { (this \ path).text.toInt }
        catch
            { case _ : Throwable => 0 }
        }

    def \!(path: String) : Boolean =
        {
        try
            { (this \ path).text.toBoolean }
        catch
            { case _  : Throwable => false }
        }

    def \\(path: String) : XpathResult =
        new XpathResult(Nil)

}




/**
 * Common trait for things that need to output Xml
 */ 
trait XmlOutput
{
    private val hex = "0123456789abcdef".toCharArray

    def xmlStr(ins: String)  =
        {
        val buf = new StringBuilder
        for (ch <- ins)
            {
            if (ch == '"')       buf.append("&quot;")
            else if (ch == '\'') buf.append("&apos;")
            else if (ch == '&')  buf.append("&amp;")
            else if (ch == '<')  buf.append("&lt;")
            else if (ch == '>')  buf.append("&gt;")
            else if ((ch > 32 && ch < 127) || ch.isWhitespace) buf.append(ch)
            else if (ch < 128)  //catch two-digit escapes
                 buf.append("&#x").
                 append(hex((ch >>  4)&0xf)).append(hex((ch      )&0xf))
            else buf.append("&#x").
                 append(hex((ch >> 12)&0xf)).append(hex((ch >>  8)&0xf)).
                 append(hex((ch >>  4)&0xf)).append(hex((ch      )&0xf))
            }
        buf.toString
        }  
}

object Xml extends XmlOutput

/**
 * This is the result of a projection.  Note that it has the same API as Node,
 * so that these can be chained together.
 */  
class XpathResult(override val self: Seq[Node]) extends scala.collection.SeqProxy[Node]
{
    def \(path: String) : XpathResult =
        new XpathResult(flatMap(_ \ path))

    def \=(path: String) : String =
        \(path).text

    def \#(path: String) : Int =
        {
        try
            { \(path).text.toInt }
        catch
            { case _  : Throwable => 0 }
        }

    def \!(path: String) : Boolean =
        {
        try
            { \(path).text.toBoolean }
        catch
            { case _  : Throwable => false }
        }

    def \\(path: String) : XpathResult =
        new XpathResult(flatMap(_ \\ path))

    def text : String =
        map(_.value).mkString
    
    def elems : Seq[Element] =
        collect{ case n : Element => n }
    
    def attrs : Seq[Attribute] =
        collect{ case n : Attribute => n }

    //return children of all members
    def children : Seq[Element] =
        elems.flatMap(_.children)
}


/**
 * Attribute of an element.    name="value"
 */ 
case class Attribute(
    val namespace : String = "",
    val prefix    : String = "",
    val name      : String = "",
    val value     : String = ""
) extends Node
{}

/**
 * XML Element.
 */ 
case class Element(
    val namespace   : String = "",
    val prefix      : String = "",
    val name        : String = "",
    val value       : String = "",
    val attributes  : Map[String, Attribute] = Map(),
    val children    : List[Element] = List()
) extends Node with XmlOutput
{
    def empty = (children.size == 0 && value.size == 0)

    override def apply(attrName: String) : String =
        {
        val attr = attributes.get(attrName)
        if (attr.isDefined)
            attr.get.value
        else
            ""
        }
    
    override def \(path: String) : XpathResult =
        {
        if (path(0) == '@')
            new XpathResult(attributes.valuesIterator.toList.filter(attr=> attr.name == path.substring(1)))
        else
            new XpathResult(children.filter(elem => path == "_" || elem.name == path))
        }

    override def \\(path: String) : XpathResult =
        {
        var xs = ListBuffer[Node]()
        if (path(0) == '@')
            xs ++= children.filter(elem=> elem.attributes.contains(path.substring(1)))
        else
            xs ++= children.filter(elem => path == "_" || elem.name == path)
        children.foreach(child => xs ++= (child \\ path))
        new XpathResult(xs.toList)
        }

    def getById(id: String) : Option[Element] =
        {
        def recurse(elems: List[Element]) : Option[Element] = elems match
            {
            case x :: xs =>
                 if (id == x("id")) Some(x)
                 else recurse(x.children).
                 orElse(recurse(xs))
            case Nil => None
            }
        recurse(this :: Nil)
        }

    def getByName(name: String) : List[Element] =
        {
        val xs = ListBuffer[Element]()
        def recurse(elem: Element) : Unit =
            {
            if (name == elem.name) xs += elem
            elem.children.foreach(recurse)
            }
        children.foreach(recurse)
        xs.toList
        }

    override def toString : String = toXml

    def toXml : String =
        {

        def elemOut(elem: Element, indent: Int, buf: StringBuilder) : Unit =
            {
            val sp = " " * indent
            buf.append(sp).append("<").append(elem.name)
            elem.attributes.valuesIterator.foreach(attr=>
                {
                buf.append(" ").append(attr.name).append("=\"").
                    append(xmlStr(attr.value)).append("\"")
                })
            if (elem.empty)
                buf.append("/>\n")
            else
                {
                buf.append(">").append(xmlStr(elem.value))    
                if (elem.children.size > 0)
                    {
                    buf.append("\n")
                    elem.children.foreach(celem => elemOut(celem, indent+4, buf))
                    buf.append(sp)
                    }
                buf.append("</").append(elem.name).append(">\n")
                }
            }

        var buf = new StringBuilder
        elemOut(this, 0, buf)
        buf.toString
        }
}


/**
 * Utilities to assist in DOM creation.
 */ 
object Element
{
    import java.lang.reflect.{Method,Modifier}

    //Cache our methods so that we do not need to scan every time
    private val cache = scala.collection.mutable.Map[Class[_], Map[String, Method]]()
    
    /**
     * Generate an Xml Element tree representing a Product.
     * This is intended for serializing case classes.  We do not try
     * to serialize everything, only Products, sequences, and a set
     * of expected primitive values.
     */           
     /*        
    def toXml(name: String, obj: Product) : Element =
        {
        def scan(clazz: Class[_]) : Map[String, Method] =
            cache.getOrElseUpdate(clazz,
                {
                val methods = clazz.getMethods.map(m=>(m.getName, m)).toMap
                val fields  = clazz.getDeclaredFields.collect
                    {case f if Modifier.isPrivate(f.getModifiers) => f.getName}.toSet
                methods.filterKeys(fields)
                })

        def toArray(name: String, arr: Seq[_]) : List[Element] =
            arr.collect{case o:AnyRef => o}.map(obj => toElement(name, obj)).toList

        //TODO: recode this with Product when it no longer sucks
        def toElement(name: String, obj: AnyRef) : Element =
            {
            val attrs = scala.collection.mutable.Map[String, Attribute]()
            val children = scala.collection.mutable.ListBuffer[Element]()
            val jobj = obj.asInstanceOf[java.lang.Object]
            for (m <- scan(jobj.getClass))
                {
                val name  = m._1
                val value = m._2.invoke(obj)
                value match
                    {
                    case v: Seq[_]   => children ++= toArray(name, v)
                    case v: Array[_] => children ++= toArray(name, v)
                    case v: Product  => children += toElement(name, v)
                    case _           => attrs += name -> new Attribute(name=name, value=value.toString)
                    }
                () // <-- workaround to inferencer bug
                }
            new Element(name = name, attributes = attrs.toMap, children = children.toList)
            }

         
        toElement(name, obj)
        }
        */
}





/**
 * Parse an XML source and return an Element
 */ 
class XmlReader extends org.xml.sax.helpers.DefaultHandler
{
    var root : Option[Element] = None
    
    //Things to save on the stack for each element
    class StackItem
        {
        val attrs    = scala.collection.mutable.Map[String, Attribute]()
        val children = scala.collection.mutable.ListBuffer[Element]()
        val buf      = new StringBuilder        
        }
    val stack = scala.collection.mutable.Stack[StackItem]()

    /**
     * from sax Handler
     */
    override def startElement(uri: String, localName: String, qName: String,
                        jattrs: org.xml.sax.Attributes) =
        {
        val item = new StackItem
        for (i <- 0 until jattrs.getLength)
            {
            val key = jattrs.getLocalName(i)
            item.attrs += key -> Attribute(name = key, value = jattrs.getValue(i))
            }
        stack.push(item)        
        }

    /**
     * from sax Handler
     */
    override def characters(ch: Array[Char], start: Int, length: Int) =
        stack.top.buf.appendAll(ch, start, length)

    /**
     * from sax Handler
     */
    override def endElement(uri: String, localName: String, qName: String) =
        {
        val item = stack.pop
        val elem = Element(
            name       = localName, 
            attributes = item.attrs.toMap,
            children   = item.children.toList,
            value      = item.buf.toString.trim
            )
        root = Some(elem)
        if (stack.size > 0)
            stack.top.children += elem
        }

    def parse(str: String) : Option[Element] =
        {
        val parser = org.xml.sax.helpers.XMLReaderFactory.createXMLReader
        parser.setContentHandler(this)
        val src = new org.xml.sax.InputSource(new java.io.StringReader(str))
        try
            {
            parser.parse(src)
            root
            }
        catch
            {
            case e: Exception => pedro.log.error("parse:" + e) ; None
            }
        }

    def parseFile(fname: String) : Option[Element] =
        {
        try
            {
            parse(scala.io.Source.fromFile(fname).mkString)
            }
        catch
            {
            case e: Exception => pedro.log.error("parseFile:" + e) ; None
            }
        }

    def parseURL(urls: String) : Option[Element] =
        {
        try
            {
            parse(scala.io.Source.fromURL(urls).mkString)
            }
        catch
            {
            case e: Exception => pedro.log.error("parseURL:" + e) ; None
            }
        }
}


/**
 * Simple object for parsing XML to an Element
 */ 
object XmlReader
{
    def parse(buf: String) : Option[Element] =
        (new XmlReader).parse(buf)

    def parseURL(urls: String) : Option[Element] =
        (new XmlReader).parseURL(urls)

    def parseFile(urls: String) : Option[Element] =
        (new XmlReader).parseFile(urls)
}



/**
 * Although this is called "push," what this means is the parser processes
 * one character at a time, sensing when a certain XML element nesting depth
 * has been completed.  Normally this is 1 for things like XMPP stanzas, but
 * it can be adjusted according to needs.  So either you can read one character
 * at a time until you have an Element, or simply read characters and allow
 * a callback to be invoked.     
 * Examples:
 *
 * If you are expecting balanced XML like <a><b></b></a> and want to parse it all,
 * then simply use:
 *     new XmlPush
 *
 * If you are expecting <stream:stream {xmlns and version info}> and want to catch it then use:
 *     new XmlPush(1, "</stream:stream>", "", true)
 *
 * If you are expecting <stream:features>stuff</stream:features> , you will need the "stream"
 *    prefix to be declared.  Do it like this:
 *     new XmlPush(addNS = "xmlns:stream='http://etherx.jabber.org/streams'")
 *
 * @param level At what depth do we want to trigger parsing and returning a value?
 * @param suffix this is what would be requred to close the xml properly if level > 0
 * @param addNS since we are processing xml in chunks, it is sometimes possible that
 *    a namespace prefix might be used that is declared elsewhere.  This allows you to
 *    have that little bit extra information added to the outermost open tag to make
 *    the parser happy.  Specify this with length>0 to use it.
 * @param onOpenTag do we want to trigger parsing when the open tag is completed at the
 *   desired depth, or when the close tag is completed?
 */ 
class XmlPush(level: Int = 0, suffix: String = "",
     addNS: String = "", onOpenTag : Boolean = false) extends pedro.util.Logged
{
    var depth        = 0      // how nested in tags?
    var inComment    = false  // we are in <!-- -->
    var inTag        = false  // we are in <name> or </name>
    var slashSeen    = false  // / seen in </name>
    var ignorable    = false  // <? stuff ?>
    var quoteChar    = 0      // " or ''
    var textSeen     = false  // seen text inside a tag
    var trivialTag   = false  // if it is <name/>
    
    var firstTag     = true
    
    val buf = new StringBuilder
    var count = 0

    def out(str: String) : Option[Element] =
        {
        println("parse:" + str)
        val elem = (new XmlReader).parse(str)
        elem
        }
        
        
    /**
     *  This is the state machine.  Please understand this
     *  before modifying.     
     */
    def append(chr: Int) : Option[Element] =
        {
        if (chr < 0)
            return None
        val ch = chr.toChar
        //print(ch)
        count += 1
        buf.append(ch)
        if (inComment)
            {
            if (count >= 3 && buf.lastIndexOf("-->") == count-3)
                {
                inComment = false
                }
            }
        else
            {
            if (count >= 4 && buf.lastIndexOf("<!--") == count-4)
                {
                inComment = true
                }
            else if (ch == '<')
                { //do a reset for the new tag
                inTag      = true
                slashSeen  = false
                ignorable  = false
                quoteChar  = 0
                textSeen   = false
                trivialTag = false
                }
            else if (ch == '>')
                {
                if (!inTag)  //unescaped '>' in pcdata? horror
                    {//todo:  report error?
                    return None
                    }
                
                if (firstTag && !ignorable && addNS.length > 0)
                    {
                    var pos = if (trivialTag) buf.lastIndexOf("/") else count-1
                    buf.insert(pos, " " + addNS)
                    firstTag = false
                    }

                inTag = false
                if (trivialTag || ignorable)
                    {//no incr or decr
                    }
                else if (slashSeen)//close tag
                    depth -= 1
                else
                    depth += 1
                //println("depth: " + depth)
                if (depth <= level && !onOpenTag)
                    {
                    val res = buf.append(suffix).toString
                    return out(res)
                    }
                else if (depth >= level && onOpenTag)
                    {
                    val res = buf.append(suffix).toString
                    return out(res)
                    }
                }
            else if (ch == '/')
                {
                if (inTag && quoteChar == 0)
                    {
                    slashSeen = true
                    if (textSeen) // <tagName/>  <--looks like this
                        trivialTag = true
                    }
                }
            else if (ch == '?')
                {
                if (inTag && quoteChar == 0)
                    ignorable = true
                }
            else if (ch == '"' || ch == '\'')
                {
                if (inTag)
                    if (quoteChar == 0) //open quote
                        quoteChar = ch
                    else if (quoteChar == ch) //close quote match the open?
                        quoteChar = 0
                }
            else
                {
                if (inTag && quoteChar == 0 && !ch.isWhitespace)
                    textSeen = true
                }
            }// if inComment

        None
        }

}

class XmlPush2 extends org.xml.sax.helpers.DefaultHandler with pedro.util.Logged
{
    self =>

    //Things to save on the stack for each element
    class StackItem(val name: String)
        {
        val attrs    = scala.collection.mutable.Map[String, Attribute]()
        val children = scala.collection.mutable.ListBuffer[Element]()
        val buf      = new StringBuilder        
        }
    val stack = scala.collection.mutable.Stack[StackItem]()

    /**
     * from sax ContentHandler
     */
    override def startElement(uri: String, localName: String, qName: String,
                     jattrs: org.xml.sax.Attributes) =
        {
        val item = new StackItem(localName)
        for (i <- 0 until jattrs.getLength)
            {
            val key = jattrs.getLocalName(i)
            item.attrs += key -> Attribute(name = key, value = jattrs.getValue(i))
            }
        stack.push(item)        
        }

    /**
     * from sax ContentHandler
     */
    override def characters(ch: Array[Char], start: Int, length: Int) =
        stack.top.buf.appendAll(ch, start, length)

    /**
     * from sax ContentHandler
     */
    override def endElement(uri: String, localName: String, qName: String) =
        {
        val item = stack.pop
        val elem = Element(
            name       = item.name, 
            attributes = item.attrs.toMap,
            children   = item.children.toList,
            value      = item.buf.toString.trim
            )
        if (!process(elem) && stack.size > 0)
            stack.top.children += elem
        }
        
    def depth = stack.size
    
    def process(elem: Element) : Boolean =
        {
        if (depth == 1)
            {
            println("elem:" + elem)
            true
            }
        else
            false
        }

    var outs = new java.io.PipedOutputStream
    var ins = new java.io.PipedInputStream(outs)
    var running = false
    
    class Receiver extends Thread
        {
        override def run =
            {
            running = true
            val parser = org.xml.sax.helpers.XMLReaderFactory.createXMLReader
            parser.setContentHandler(self)
            try
                {
                val rdr = new java.io.InputStreamReader(ins, "UTF-8")
                val src = new org.xml.sax.InputSource(rdr)
                src.setEncoding("UTF-8")
                parser.parse(src)
                }
            catch 
                {
                case e: Exception =>
                    trace("XmlPush: " + e)
                }
            }
        }
    private var receiver = new Receiver


    def append(ch: Int) =
        {
        if (running)
            {
            outs.write(ch)
            outs.flush
            }
        }
        
    def start =
        {
        outs = new java.io.PipedOutputStream
        ins = new java.io.PipedInputStream(outs)
        receiver = new Receiver
        receiver.start
        }
        
    def close =
        {
        running = false
        outs.close
        //ins.close
        }
           
}


object XmlPush2Test
{

    def test =
        {
        val p = new XmlPush2
            {
            override def process(elem: Element) : Boolean =
                {
                if (stack.top.name == "root")
                    {
                    println("elem:" + elem)
                    true
                    }
                else
                    false
                }
            }

        val xml = "<root><a/><b/><c/></root>".getBytes("UTF-8").map(_.toInt & 255)
        p.start
        for (i <- xml)
            p.append(i)
        Thread.sleep(2000)
        p.close
        
        }
        
        
    def main(argv: Array[String]) : Unit =
        {
        test
        }

}



class XmlPush3 extends Thread
{
    private val outs = new java.io.PipedOutputStream
    private val ins  = new java.io.PipedInputStream(outs)
    
    //Things to save on the stack for each element
    class StackItem(val name: String, val attrs: Map[String, Attribute])
        {
        val children = scala.collection.mutable.ListBuffer[Element]()
        val buf      = new StringBuilder        
        }
    val stack = scala.collection.mutable.Stack[StackItem]()
    
    //stax api does not use generics
    private def getAttrs(iter: java.util.Iterator[_]) : Map[String, Attribute] =
        {
        val buf = scala.collection.mutable.Map[String, Attribute]()
        while (iter.hasNext)
            {
            val jattr = iter.next.asInstanceOf[javax.xml.stream.events.Attribute]
            val name = jattr.getName.toString
            buf += name -> Attribute(name, jattr.getValue)
            }
        buf.toMap
        }
   
    private var open = true
    
    override def run : Unit =
        {
        val inputFactory = javax.xml.stream.XMLInputFactory.newInstance
        var rdr = new java.io.InputStreamReader(ins, "UTF-8")
        val parser = inputFactory.createXMLEventReader(rdr)
        while (open && parser.hasNext)
            {
            val evt = parser.nextEvent
            if (evt.isStartElement)
                {
                val elem = evt.asStartElement
                stack.push(new StackItem(elem.getName.toString, getAttrs(elem.getAttributes)))        
                }
            else if (evt.isCharacters)
                {
                stack.top.buf.append(evt.asCharacters.getData)
                }
            else if (evt.isEndElement)
                {
                val endElem = evt.asEndElement
                val item = stack.pop
                val elem = Element(
                    name       = item.name, 
                    attributes = item.attrs,
                    children   = item.children.toList,
                    value      = item.buf.toString.trim
                    )
                if (!process(elem) && stack.size > 0)
                    stack.top.children += elem
                }
                
            }
        }

    def depth = stack.size
    
    def process(elem: Element) : Boolean =
        {
        if (depth == 1)
            {
            println("elem:" + elem)
            true
            }
        else
            false
        }
        
    def append(ch: Int) =
        {
        if (open)
            {
            outs.write(ch)
            outs.flush
            }
        }
        
    def close =
        {
        open = false
        outs.close
        }
    
}



object XmlPush3Test
{

    def test =
        {
        val p = new XmlPush3
            {
            override def process(elem: Element) : Boolean =
                {
                if (stack.size > 0 && stack.top.name == "root")
                    {
                    println("elem:" + elem)
                    true
                    }
                else
                    false
                }
            }

        val xml = "<root><a/><b/><c/></root>".getBytes("UTF-8").map(_.toInt & 255)
        for (i <- xml)
            p.append(i)
        p.start
        }
        
        
    def main(argv: Array[String]) : Unit =
        {
        test
        }

}


