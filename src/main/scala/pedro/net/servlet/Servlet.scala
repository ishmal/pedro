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
        self.setAttribute(key, value)
}



class Request(val self : HttpServletRequest)
{
    val reader = self.getReader

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
    val writer = self.getWriter

    def +(msg: String) : Response =
        {
        writer.write(msg)
        this
        }
    
    def sendError(code: Int, msg: String) =
        self.sendError(code, msg)
}



class Servlet extends HttpServlet
{
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

    override def service(req: HttpServletRequest, resp: HttpServletResponse) =
        {
        val outs = resp.getWriter

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
        outs.flush
        }

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




