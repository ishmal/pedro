/**
 *  This is a simple implementation of OAuth for Scala.
 *
 *  Author: Bob Jamison
 *  Copyright, 2010 
 *    
 *  
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */   

package pedro.net.servlet


import java.io.{Reader,Writer}
import javax.servlet.http._



/**
 * Yes, there have been scala wrappers of Servlet stuff before, with 
 * limited success. The purpose of this package is twofold:
 *  o   allow the coder to use only Scala 
 *  o   reduce the amount of code necessary
 */

class Session(val self: HttpSession)
{
    def apply(key: String) : Option[Any] =
        Option(self.getAttribute(key))

    def update(key: String, value: Any) =
        { //convert None back to null.  damn java
        val cvalue = if (value == None) null else value
        self.setAttribute(key, cvalue)
        }
}



class Request(val self : HttpServletRequest, val auth:Auth = AuthNone)
{
    lazy val reader = self.getReader

    //useful for PUT
    def read : String =
        {
        val buf = new java.io.ByteArrayOutputStream
        val ins = self.getInputStream
        var ch = ins.read
        while (ch >= 0)
            {
            buf.write(ch)
            ch = ins.read
            }
        new String(buf.toByteArray)
        }

    val parameters =
        {
        val parms  = scala.collection.mutable.Map[String,String]()
        val names  = self.getParameterNames
        while (names.hasMoreElements)
            {
            val name = names.nextElement.asInstanceOf[String]
            parms   += name -> self.getParameter(name)
            }
        parms.toMap.withDefaultValue("")
        }

    lazy val session = new Session(self.getSession)
    
    lazy val pathInfo    = self.getPathInfo
    lazy val servletPath = self.getServletPath

    def apply(key: String) =
        parameters(key)
        
}



class Response(val self : HttpServletResponse)
{
    
    def setContentLength(len: Int) =
        self.setContentLength(len)

    def setContentType(typ: String) =
        self.setContentType(typ)

    def +(msg: String) : Response =
        {
        self.getOutputStream.write(msg.getBytes)
        this
        }
    
    def +(msg: Array[Byte]) : Response =
        {
        self.getOutputStream.write(msg)
        this
        }

    def sendError(code: Int, msg: String) =
        self.sendError(code, msg)
}


/**
 * This class wraps a response that we pass to a servlet.  It
 * buffers the output of the previous member (servlet or filter) of the filter chain,
 * which we can then post-process.  This avoids the needs for pipes.
 * Example:
 *       val buf = new BufferedResponse(currentResponse)
 *       filterChain.filter(request, buf)
 *       val bytes = buf.get  
 */  
class BufferedResponse(selfArg : HttpServletResponse) extends Response(selfArg)
{
    private val baos = new java.io.ByteArrayOutputStream

    def get =
        baos.toByteArray

    override val self = new javax.servlet.http.HttpServletResponseWrapper(selfArg)
        {
        override def getOutputStream =
            new javax.servlet.ServletOutputStream
                {
                override def write(ch:Int) = baos.write(ch)
                }
        }
}


/**
 * This subclass of Response will buffer the output of the previous member of the
 * FilterChain.   It will also compute a hash during buffering in order to produce
 * one efficiently at the end of buffering.  Having a computed hash of what's in the
 * buffer is an aid in caching.   
 * Example:
 *       val buf = new BufferedResponse(currentResponse)
 *       filterChain.filter(request, buf)
 *       val bytes = buf.get  
 *       val hash  = buf.hash 
 */  
class HashedBufferedResponse(selfArg : HttpServletResponse) extends Response(selfArg)
{
    private val baos = new java.io.ByteArrayOutputStream

    private val md = java.security.MessageDigest.getInstance("SHA")
    
    def hash = md.digest

    def get = baos.toByteArray

    override val self = new javax.servlet.http.HttpServletResponseWrapper(selfArg)
        {
        override def getOutputStream =
            new javax.servlet.ServletOutputStream
                {
                override def write(ch:Int) = { md.update(ch.toByte) ; baos.write(ch) }
                }
        }

}



class Servlet extends HttpServlet
{
    /**
     * Escape XML entities.  Should use this for any unknown
     * strings.     
     */         
    def xmlStr(ins: String) : String =
        {
        val buf = new StringBuilder
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
        buf.toString
        }

    def realPath(fname: String) =
        getServletContext.getRealPath(fname)

    /**
     * Can be overridden
     */     
    def doPut(in: Request, out: Response) =
        {
        }

    /**
     * Can be overridden
     */     
    def doPost(in: Request, out: Response) =
        {
        }

    /**
     * Can be overridden
     */     
    def doGet(in: Request, out: Response) =
        {
        }

    /**
     * Can be overridden
     */     
    def doDelete(in: Request, out: Response) =
        {        
        }

    /**
     * Override this for your application-specific authorization
     */         
    def authorize(req: Request, resp: Response) : Option[Auth] =
        None

    def getAuth(req: HttpServletRequest) : Option[Auth] =
        {
        val obj = Some(req.getSession.getAttribute("auth"))
        obj.collect{case a:Auth => a}
        }
    
    override def service(req: HttpServletRequest, resp: HttpServletResponse) =
        {
        val newreq  = new Request(req)
        val newresp = new Response(resp)

        req.getMethod match
            {
            case "PUT"    => doPut(newreq, newresp)
            case "POST"   => doPost(newreq, newresp)
            case "GET"    => doGet(newreq, newresp)
            case "DELETE" => doDelete(newreq, newresp)
            case _ =>
                {
                //405 is "method not allowed"
                resp.sendError(405, "unsupported HTTP method '" + req.getMethod + "'")
                }
            }
        resp.getOutputStream.flush
        }

    //slow, but only happens once at init() time
    lazy val initParameters = 
        {
        val parms = scala.collection.mutable.Map[String,String]()
        val names = getInitParameterNames
        while (names.hasMoreElements)
            {
            val name = names.nextElement.asInstanceOf[String]
            parms += name -> getInitParameter(name)
            }  
        parms.toMap.withDefaultValue("")
        }

}


class FilterChain(val self: javax.servlet.FilterChain)
{
    def filter(request: Request, response: Response) =
        self.doFilter(request.self, response.self)        
}

class FilterConfig(val self: javax.servlet.FilterConfig)
{
    def name = self.getFilterName
    
    //slow, but only happens once at init() time
    lazy val initParameters = 
        {
        val parms = scala.collection.mutable.Map[String,String]()
        val names = self.getInitParameterNames
        while (names.hasMoreElements)
            {
            val name = names.nextElement.asInstanceOf[String]
            parms += name -> self.getInitParameter(name)
            }  
        parms.toMap.withDefaultValue("")
        }

    def apply(name: String) =  initParameters.getOrElse(name, "")
    
    def servletContext = self.getServletContext
}


class Filter extends javax.servlet.Filter
{
    //Override this
    def init(config: FilterConfig) =
        {
        
        }
    
    //Override this
    def filter(request: Request, response: Response, chain: FilterChain) =
        {
        
        }

    def forward(url: String, request: Request, response: Response) =
        request.self.getRequestDispatcher(url).forward(request.self, response.self)


    override def init(config: javax.servlet.FilterConfig) =
        init(new FilterConfig(config))
    
    override def doFilter(req: javax.servlet.ServletRequest,
        resp: javax.servlet.ServletResponse, 
        chain: javax.servlet.FilterChain)
        {
        val newreq  = new Request(req.asInstanceOf[HttpServletRequest])
        val newresp = new Response(resp.asInstanceOf[HttpServletResponse])
        val newfilt = new FilterChain(chain)
        filter(newreq, newresp, newfilt)
        }
    
    override def destroy =
        {
        }
}




