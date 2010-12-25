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


package pedro.net.servlet




import javax.servlet.ServletException



/**
 * Provide a means to transform the xml output of a servlet
 * into the xml described by an XSLT stylesheet
 */  
class XsltFilter extends Filter
{
    var filterName = ""

    var templates: Option[javax.xml.transform.Templates] = None


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

        val factory = javax.xml.transform.TransformerFactory.newInstance
        try
            {
            val reader = new java.io.FileReader(absolutePath)
            templates = Some(factory.newTemplates(new javax.xml.transform.stream.StreamSource(reader)))
            }
        catch
            {
            case ioe : java.io.IOException =>
                throw new ServletException("XsltFilter '" + filterName + "' loading file '" +
        		                absolutePath + "' :" + ioe)
            case tce : javax.xml.transform.TransformerConfigurationException =>
                throw new ServletException("XsltFilter '" + filterName + "' loading file '" +
        		                xsltFile + "' : " +
        					    tce.getMessageAndLocation)
            }

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
                
        val bufferedResp = new BufferedResponse(resp.self)
        try
            {
            chain.filter(req, bufferedResp)
            }
        catch
            {
            case ioe: java.io.IOException =>
                throw new ServletException(
                    "XsltFilter '" + filterName + "' : " + ioe)
            }
        
        //Make input and output buffered streams
        val outxml = bufferedResp.get
        dumpBuf(outxml)
        val bais = new java.io.ByteArrayInputStream(outxml)
        val transSource = new javax.xml.transform.stream.StreamSource(bais)
        val baos = new java.io.ByteArrayOutputStream
        val transResult = new javax.xml.transform.stream.StreamResult(baos)
    
        //Now transform
        try
            {
            val transformer = templates.get.newTransformer
            transformer.transform(transSource, transResult)
            }
         catch
             {
             case e : javax.xml.transform.TransformerException =>
                 throw new ServletException(
                     "XsltFilter '" + filterName + "' : " + e)
             }
        
        //Finally, output the transformed data
        try
            {
            val obuf =  baos.toByteArray
            resp.setContentLength(obuf.length)
            resp + obuf
            }
        catch
            {
            case e: java.io.IOException => 
                throw new ServletException(
                    "XsltFilter '" + filterName + "' : " + e)
            }
    
    
        }




}
//########################################################################
//# E N D    O F    F I L E
//########################################################################


