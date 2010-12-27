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


import javax.servlet.http.{HttpServletRequest,HttpServletResponse}


 
/**
 * This is code to handle REST requests for a single resource type
 */ 
trait ResourceHandler
{
    /**
     * Used for fetching an item or querying for a list of items
     * usage:   GET /resourceType/resourceName
     *    or    GET /resourceType?q="query string"          
     */         
    def doGet(req: Request, resp: Response, resourceName: String) =
        {
        }

    /**
     * Used for creating an item.
     * usage:  POST /resourceType <cr> data=value    
     */         
    def doPost(req: Request, resp: Response) =
        {
        }

    /**
     * Used for updating an item.
     * usage:  PUT /resourceType/resourceName <cr> data     
     */         
    def doPut(req: Request, resp: Response, resourceName: String) =
        {
        }

    /**
     * Used for deleting a single item.
     * usage:  PUT /resourceType/resourceName
     *      
     * It is unsafe to delete multiple objects this way.  If you want to
     * delete more than one, define a resourceType for the group, and
     * delete that.               
     */         
    def doDelete(req: Request, resp: Response, resourceName: String) =
        {
        }

}





class RestServlet extends Servlet
{
    val handlers = scala.collection.mutable.Map[String, ResourceHandler]()
    
    def add(resourceType: String, handler: ResourceHandler) : ResourceHandler =
        {
        handlers += resourceType -> handler
        handler
        }
    
    /**
     * Override this to perform authorization on your specific application
     */         
    def auth(req: Request) : Boolean =
        {
        true
        }
    
    override def service(req: HttpServletRequest, resp: HttpServletResponse)=
        {
        val outs = resp.getWriter

        val method = req.getMethod
        var pathInfo = req.getPathInfo
        if (pathInfo.length > 1 && pathInfo(0) == '/')
            pathInfo = pathInfo.substring(1)
        val words = pathInfo.split("/")
        if (words.size < 1 || words.size > 2)
            {
            //400 is "bad request" , like a syntax error
            resp.sendError(400, "invalid request path: '" + pathInfo + "'")
            //log it
            }
        else
            {
            val resourceType = words(0)
            val resourceName = if (words.size >= 2) words(1) else ""
            val handler = handlers.get(resourceType)
            if (!handler.isDefined)
                {
                //404 is the much beloved "Not found"
                resp.sendError(404, "unknown REST resource type '" + resourceType + "'")
                //log it
                }
            else
                {
                val newreq  = new Request(req)
                val newresp = new Response(resp)
                if (auth(newreq))
                    {
                    method match
                        {
                        case "PUT" =>
                            handler.get.doPut(newreq, newresp, resourceName)
                        case "POST" =>
                            handler.get.doPost(newreq, newresp)
                        case "GET" =>
                            handler.get.doGet(newreq, newresp, resourceName)
                        case "DELETE" =>
                            handler.get.doDelete(newreq, newresp, resourceName)
                        case _ =>
                            {
                            //405 is "method not allowed"
                            resp.sendError(405, "unsupported HTTP method '" + method + "'")
                            //logit("unsupported")
                            }
                        }
                    }
                else
                    {
                    //401 is "not authorized"
                    resp.sendError(401, "not authorized for api. please log in")
                    //logit("not authorized")
                    }
                }//servlet.isDefined
            }//words.size
        }


}



