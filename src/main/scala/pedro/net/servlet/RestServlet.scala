/**
 * Simple Servlet tools.
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


import javax.servlet.http.{HttpServletRequest,HttpServletResponse}


 
/**
 * This is code to handle REST requests for a single resource type.
 */ 
class RestHandler(val resourceType: String)
{
    /**
     * Used for fetching an item or querying for a list of items
     * usage:   GET /resourceType/resourceName
     *    or    GET /resourceType?q="query string"          
     */         
    def doGet(req: Request, resp: Response, resourceName: String) =
        {
        resp.sendError(405, "GET method not implemented for: '" + resourceType + "'")
        }

    /**
     * Used for creating an item.
     * usage:  POST /resourceType <cr> data=value    
     */         
    def doPost(req: Request, resp: Response) =
        {
        resp.sendError(405, "POST method not implemented for: '" + resourceType + "'")
        }

    /**
     * Used for updating an item.
     * usage:  PUT /resourceType/resourceName <cr> data     
     */         
    def doPut(req: Request, resp: Response, resourceName: String) =
        {
        resp.sendError(405, "PUT method not implemented for: '" + resourceType + "'")
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
        resp.sendError(405, "DELETE method not implemented for: '" + resourceType + "'")
        }

}






class RestServlet extends Servlet with pedro.util.Logged
{
    val handlers = scala.collection.mutable.Map[String, RestHandler]()
    
    val mimeType = "text/html"
    
    def add(handler: RestHandler) : RestHandler =
        {
        handlers += handler.resourceType -> handler
        handler
        }
    
    /**
     * Log in.  Override authorize for your needs and output the answer
     */         
    def login(req: Request, resp: Response) =
        {
        val authRec = authorize(req, resp)
        if (authRec.isDefined)
            req.session("auth") = authRec.get
        }

    /**
     * Log out.  Override this to output the http answer. Call
     * super first.     
     */         
    def logout(req: Request, resp: Response) =
        {
        req.session("auth") = None
        }

    add(new RestHandler("login")
        {
        override def doPost(req: Request, resp: Response) = login(req, resp)
        override def doGet(req: Request, resp: Response, resourceName:String) = doPost(req, resp)
    
        override def doDelete(req: Request, resp: Response, resourceName: String) =
            logout(req,resp)
        })
    
    override def service(req: HttpServletRequest, resp: HttpServletResponse) =
        {
        val method = req.getMethod.toLowerCase
        var pathInfo = if (req.getPathInfo != null) req.getPathInfo else ""
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
                val auth = authorize(newreq, newresp)
                if (auth.isDefined || resourceType == "login")
                    {
                    val h = handler.get
                    resp.setContentType(mimeType)
                    method match
                        {
                        case "put" =>
                            h.doPut(newreq, newresp, resourceName)
                        case "post" =>
                            h.doPost(newreq, newresp)
                        case "get" =>
                            h.doGet(newreq, newresp, resourceName)
                        case "delete" =>
                            h.doDelete(newreq, newresp, resourceName)
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



