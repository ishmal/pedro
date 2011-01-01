/**
 * Sands small business data assistant.
 *
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (C) 2011 Bob Jamison
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


package pedro.net.servlet


import javax.servlet.ServletException



/**
 * Provide a means to transform the xml output of a servlet
 * into the xml described by an XSLT stylesheet
 */  
class XsltFilter extends Filter with pedro.util.Logged
{
    var filterName = ""

    var templates: Option[javax.xml.transform.Templates] = None

    private val factory = javax.xml.transform.TransformerFactory.newInstance
 
    /**
     * Called by the web container to indicate to a filter
     * that it is being placed into service.
     */
    override def init(config: FilterConfig) =
        {
        filterName       = config.name
        val xsltFile     = config("xsltFile")
        val context      = config.servletContext
        val absolutePath = context.getRealPath(xsltFile)
        
        if (xsltFile.length == 0)
            throw new ServletException("XsltFilter '" +
                filterName + "' requires 'xsltFile' parameter")

        templates = loadFile(absolutePath).flatMap(loadTemplate)
        }

    def dumpBuf(buf: Array[Byte]) =
        {
        try
            {
            val dump = new java.io.FileOutputStream("dump.txt")
            dump.write(buf)
            dump.close
            }
        catch
            {
            case e: java.io.IOException =>
            }
        }

    def loadTemplate(templ: String) : Option[javax.xml.transform.Templates] =
        {
        try
            {
            val bais = new java.io.ByteArrayInputStream(templ.getBytes)
            Some(factory.newTemplates(new javax.xml.transform.stream.StreamSource(bais)))
            }
        catch
            {
            case e : javax.xml.transform.TransformerConfigurationException =>
                    error("XsltFilter loadTemplate: " + e.getMessageAndLocation)
                None
            }
        
        }
    
    def loadFile(fname: String) : Option[String] =
        {
        try
            {
            Some(scala.io.Source.fromFile(fname)("UTF-8").mkString)
            }
        catch
            {
            case e : java.io.IOException => error("loadTempleteFile: " + e)
                None
            }
        }
    
    /**
     * Public, so it's testable    
     */     
    def transform(inb: Array[Byte]) : Option[Array[Byte]] =
        {
        val bais   = new java.io.ByteArrayInputStream(inb)
        val source = new javax.xml.transform.stream.StreamSource(bais)
        val baos   = new java.io.ByteArrayOutputStream
        val result = new javax.xml.transform.stream.StreamResult(baos)
    
        //Transform
        try
            {
            val transformer = templates.get.newTransformer
            transformer.transform(source, result)
            Some(baos.toByteArray)
            }
         catch
             {
             case e : javax.xml.transform.TransformerException =>
                 throw new ServletException(
                     "XsltFilter '" + filterName + "' : " + e)
             None
             }
        }
    
    //caching.
    var lastOut  = Array[Byte]()
    var lastHash = Array[Byte]()

    /**
     * The doFilter method of the Filter is called by the container each time
     * a request/response pair is passed through the chain due to a client
     * request for a resource at the end of the chain.
     */
    override def filter(req: Request, resp: Response, chain: FilterChain) =
        {
        if (!templates.isDefined)
            throw new javax.servlet.ServletException("XsltFilter '" + filterName + 
    		          "' cannot be used.  It was not configured properly")
                
        val bufferedResp = new HashedBufferedResponse(resp.self)
        chain.filter(req, bufferedResp)

        //Make input and output buffered streams
        val rawxml = bufferedResp.get
        val hash = bufferedResp.hash
        if (java.security.MessageDigest.isEqual(lastHash, hash))
            resp + lastOut
        else
            {
            //dumpBuf(rawxml)
            val transxml = transform(rawxml)
            if (transxml.isDefined)
                {
                resp.setContentLength(transxml.get.length)
                resp + transxml.get
                lastOut = transxml.get
                lastHash = hash
                }
            }
        }



}
//########################################################################
//# E N D    O F    F I L E
//########################################################################


