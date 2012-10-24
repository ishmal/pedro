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
 * Handler for the Java SAX parser's 3 callbacks
 */ 
class Handler(parent: XmlReader) extends org.xml.sax.helpers.DefaultHandler
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

    override def characters(ch: Array[Char], start: Int, length: Int) =
        stack.top.buf.appendAll(ch, start, length)

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
}


/**
 * Parse an XML source and return an Element
 */ 
class XmlReader
{
    def parse(str: String) : Option[Element] =
        {
        val parser = org.xml.sax.helpers.XMLReaderFactory.createXMLReader
        val handler = new Handler(this)
        parser.setContentHandler(handler)
        val src = new org.xml.sax.InputSource(new java.io.StringReader(str))
        try
            {
            parser.parse(src)
            handler.root
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
 */ 
class XmlPush extends pedro.util.Logged
{
    var depth        = 0      // how nested in tags?
    var inComment    = false  // we are in <!-- -->
    var commentDelim = 0
    var inTag        = false  // we are in <name> or </name>
    var slashSeen    = false  // / seen in </name>
    var querySeen    = false  // <?
    var quoteChar    = 0      // " or ''
    var textSeen     = false  // seen text inside a tag
    var trivialTag   = false  // if it is <name/>
    
    val buf = new StringBuilder

    private def resetState =
        {
        inComment    = false
        commentDelim = 0
        inTag        = true
        slashSeen    = false
        querySeen    = false
        quoteChar    = 0
        textSeen     = false
        trivialTag   = false
        }

    private def reset
        {
        resetState
        buf.clear
        depth = 0
        }
    
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
    def append(chr: Int, threshold: Int=0, suffix: String = "") : Option[Element] =
        {
        val ch = chr.asInstanceOf[Char]
        print(ch)
        buf.append(ch)
        if (inComment)
            {
            if (ch == '-' && commentDelim==0)
                commentDelim = 1
            else if (ch == '-' && commentDelim==1)
                commentDelim = 2
            else if (ch == '>' && commentDelim==2)
                { inComment = false ; commentDelim = 0}
            else
                commentDelim = 0
            }
        else
            {
            if (ch == '<')
                { if (depth==threshold-1) {buf.clear; buf.append('<')} ; resetState ; commentDelim = 1}
            else if (ch == '!' && commentDelim == 1)
                commentDelim = 2
            else if (ch == '-' && commentDelim == 2)
                commentDelim = 3
            else if (ch == '-' && commentDelim == 3)
                { inComment = true ; commentDelim = 0 }
            else if (ch == '>')
                {
                if (!inTag)  //unescaped '>' in pcdata? horror
                    return None
                inTag = false
                if (!trivialTag && !querySeen)
                    if (slashSeen)
                        depth -= 1
                    else 
                        depth += 1 
                println("depth: " + depth)
                if (depth <= threshold && !querySeen)
                    {
                    val res = buf.append(suffix).toString
                    reset
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
                    querySeen = true
                }
            else if (ch == '"' || ch == '\'')
                {
                if (inTag)
                    if (quoteChar == 0)
                        quoteChar = ch
                    else if (quoteChar == ch)
                        quoteChar = 0
                }
            else
                {
                commentDelim = 0
                if (inTag && quoteChar == 0 && !ch.isWhitespace)
                    textSeen = true
                }
            }// if inComment

        None
        }

}

/**
 * Convenience object for calling the push parser
 */ 
object XmlPush
{
    def parse(str: String = "", depth:Int = 1) : Boolean =
        {
        val parser = new XmlPush
        str.foreach(ch=> parser.append(ch, depth))
        true
        }

    def parseFile(fname: String, depth:Int = 1) : Boolean =
        {
        val parser = new XmlPush
        scala.io.Source.fromFile(fname).foreach(ch=> parser.append(ch, depth))
        true
        }
    
    def main(argv: Array[String]) : Unit =
        parseFile("test.xml")

}



