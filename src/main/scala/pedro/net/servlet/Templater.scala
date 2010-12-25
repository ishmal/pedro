/**
 * Simple tag substitution module
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (C) 2010 Bob Jamison
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


package pedro.net.servlet


import org.w3c.dom._


//########################################################################
//# XML PROCESSING
//########################################################################


/**
 * Handler for the Java SAX parser's 3 callbacks
 */ 
class Handler(parent: Templater) extends org.xml.sax.ext.DefaultHandler2
{
    val buf = new StringBuilder
    
    var locator : Option[org.xml.sax.Locator] = None
    override def setDocumentLocator(locator: org.xml.sax.Locator) =
        this.locator = Some(locator) 
    

    private val hex = "0123456789abcdef".toCharArray

    def xmlStr(ins: String)  =
        {
        ins.foreach(ch=>
            {
            if (ch == '"')                 buf.append("&quot;")
            else if (ch == '\'')           buf.append("&apos;")
            else if (ch == '&')            buf.append("&amp;")
            else if (ch == '<')            buf.append("&lt;")
            else if (ch == '>')            buf.append("&gt;")
            else if ((ch > 32 && ch < 127) || ch.isWhitespace) buf.append(ch)
            else if (ch < 128)  //catch two-digit escapes
                 buf.append("&#x").
                 append(hex((ch >>  4)&0xf)).append(hex((ch      )&0xf))
            else buf.append("&#x").
                 append(hex((ch >> 12)&0xf)).append(hex((ch >>  8)&0xf)).
                 append(hex((ch >>  4)&0xf)).append(hex((ch      )&0xf))
            })
        }

    var inCDATA = false
    override def startCDATA =
        {
        buf.append("<![CDATA[")
        inCDATA = true
        }

    override def endCDATA =
        {
        buf.append("]]>")
        inCDATA = false
        }
    
    override def comment(ch: Array[Char], start: Int, length: Int) =
        {
        buf.append("<!--")
        xmlStr(new String(ch, start, length))
        buf.append("-->")
        }

    override def startElement(uri: String, localName: String, qName: String,
                     jattrs: org.xml.sax.Attributes) =
        {
        val mp = scala.collection.mutable.Map[String, String]()
        for (i <- 0 until jattrs.getLength)
            mp += (jattrs.getLocalName(i) -> jattrs.getValue(i))
        val attrs = mp.toMap.withDefaultValue("")
        if (uri == parent.namespace) //Is this in our namespace? "pedro.tags"
            {
            val col = if (locator.isDefined) locator.get.getColumnNumber else 0
            val xmlSnippet = parent.apply(localName, attrs, col) //THIS IS THE MAGIC
            buf.append(xmlSnippet)    
            }
        else if (qName != "root")
            {
            buf.append("<")
            xmlStr(qName)
            attrs.foreach(e=>
                {
                buf.append(" ")
                xmlStr(e._1)
                buf.append("=\"")
                xmlStr(e._2)
                buf.append("\"")
                })
            buf.append(">")
            }
        }

    override def characters(ch: Array[Char], start: Int, length: Int) =
        if (inCDATA)
            buf.appendAll(ch, start, length)
        else
            xmlStr(new String(ch, start, length))

    override def endElement(uri: String, localName: String, qName: String) =
        {
        if (uri != parent.namespace && qName != "root")
            {
            buf.append("</")
            xmlStr(qName)
            buf.append(">")
            }
        }
}



/**
 * Read a file or buffer of XML and perform any possible substitutions
 * according to tags.  Reference back to the parent Tags registry in order
 * to use its namespace and process() method.
 */  
class Templater(parent: Tags) extends pedro.util.Logged
{
    val namespace = parent.namespace
    
    //This is the method that actually grabs the xml snippet for the tag
    def apply(name: String, attrs: Map[String, String], col: Int) : String =
        parent.apply(name, attrs, col)

    def process(buf: String) : Option[String] =
        {
        val parser = org.xml.sax.helpers.XMLReaderFactory.createXMLReader
        parser.setFeature("http://xml.org/sax/features/validation", false)
        parser.setFeature("http://xml.org/sax/features/namespaces", true)
        parser.setFeature("http://xml.org/sax/features/namespace-prefixes", true)
        val handler = new Handler(this)
        parser.setContentHandler(handler)
        parser.setEntityResolver(new org.xml.sax.EntityResolver
            {
            override def resolveEntity(publicId: String, systemId: String) =
                new org.xml.sax.InputSource(new java.io.StringReader(""))
   
            })
        val src = new org.xml.sax.InputSource(new java.io.StringReader(buf))
        try
            {
            parser.parse(src)
            Some(handler.buf.toString)
            }
        catch
            {
            case e: Exception =>
                error("process:" + e)
                if (handler.locator.isDefined)
                    error("Line: " + handler.locator.get.getLineNumber)
            None
            }
        }
    
    def processFile(fname: String) : Option[String] =
        {
        try
            {
            //println("fname: " + fname)
            val buf = scala.io.Source.fromFile(fname).mkString
            process(buf)
            }
        catch
            {
            case e: Exception => error("processFile: " + e)
            None
            }
        }    

    def processStream(ins: java.io.InputStream) : Option[String] =
        {
        try
            {
            val buf = scala.io.Source.fromInputStream(ins).mkString
            process(buf)
            }
        catch
            {
            case e: Exception => error("processStream: " + e)
            None
            }
        }    

}




