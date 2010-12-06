/**
 * Sands small business data assistant.
 *
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (C) 2010 Bob Jamison
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
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


import javax.xml.transform.stream.{StreamSource, StreamResult}




/**
 * A wrapper to provide on-the-fly xsm transformation of servlet
 * output
 */  
class XslTransformer(stylesheet: String)
{
    private val source =
        new StreamSource(new java.io.ByteArrayInputStream(stylesheet.getBytes))

    private val transformer =
        javax.xml.transform.TransformerFactory.newInstance.newTransformer(source)

    def transform(ins: java.io.InputStream, outs: java.io.OutputStream) =
        {
        try
            {
            transformer.transform(new StreamSource(ins), new StreamResult(outs))
            }
        catch
            {
            case e: Exception => println("XslTransformer: " + e) // log me
            }
        transformer.reset
        }

    def transform(ins: java.io.Reader, outs: java.io.Writer) =
        {
        try
            {
            transformer.transform(new StreamSource(ins), new StreamResult(outs))
            }
        catch
            {
            case e: Exception => println("XslTransformer: " + e) // log me
            }
        transformer.reset
        }

    def transform(instr: String) : String =
        {
        val ins = new java.io.ByteArrayInputStream(instr.getBytes)
        val outs = new java.io.ByteArrayOutputStream
        transform(ins, outs)
        outs.toString("UTF-8")
        }

}




class XslWriter(out: java.io.Writer, xsl: String) extends java.io.PrintWriter(out)
{
    def this(out: java.io.OutputStream, xsl: String) =
        this(new java.io.OutputStreamWriter(out), xsl)

    private val transformer = new XslTransformer(xsl)

    private val buf = new StringBuilder
    
    /**
     * Next 3 overrides required by Writer
     */         
    override def write(chars: Array[Char], off: Int, len: Int) =
        buf.appendAll(chars, off, len)
        
    override def flush =
        out.flush

    override def close =
        {
        val xmlStr = buf.toString
        print("str: '" + xmlStr + "'")
        val str = transformer.transform(xmlStr)
        out.write(str)
        out.close
        }

    /**
     * Override these PrintWriter output methods that use 'out'
     */         
    override def write(ch: Int) =
        buf.append(ch.asInstanceOf[Char])

    override def write(str: String, offset: Int, count: Int) =
        buf.appendAll(str.toCharArray, offset, count)

}


object XslTransformer
{

    val xsl = """
        <xsl:stylesheet xmlns:xsl="http://www.w3.org/1999/XSL/Transform">
        
        <xsl:output method="html"/>
        
        <xsl:template match="poem">
          <html><body>
            <xsl:apply-templates/>
          </body></html>
        </xsl:template>
        
        <xsl:template match="title">
          <h1><xsl:apply-templates/></h1>
        </xsl:template>
        
        <xsl:template match="excerpt">
          <p><xsl:apply-templates/></p>
          <hr></hr>
        </xsl:template>
        
        <xsl:template match="verse">
          <xsl:apply-templates/><br/>
        </xsl:template>
        
        </xsl:stylesheet>
        """


    val xml = """
        <poem><title>From Book I</title>
        <excerpt>
        <verse>Then with expanded wings he steers his flight</verse>
        <verse>Aloft, incumbent on the dusky Air</verse>
        <verse>that felt unusual weight, till on dry Land</verse>
        <verse>He lights, if it were Land that ever burne'd</verse>
        <verse>With solid, as the Lake with liquid fire;</verse>
        </excerpt>
        <excerpt>
        <verse>For who can yet believe, though after loss</verse>
        <verse>That all these puissant Leginos, whose exile</verse>
        <verse>Hath emptied Heav'n, shall fail to re-ascend</verse>
        <verse>Self-rais'd, and repossess their native seat.</verse>
        </excerpt>
        </poem>
"""


    def doTest =
        {
        val baos = new java.io.ByteArrayOutputStream
        val out = new XslWriter(baos, xsl)
        out.write(xml)
        out.close
        println(baos.toString)
        }

    def main(argv: Array[String]) =
        doTest

}