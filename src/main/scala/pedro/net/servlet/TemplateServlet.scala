/**
 * Tiny web templating package
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (C) 2011 Bob Jamison
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


class TemplateServlet extends Servlet
{
    def showTemplate(path: String, resp: Response) =
        {
        //println("path: " + path)
        val ins = Option(getServletContext.getResourceAsStream(path))
        if (ins.isEmpty)
            resp.sendError(404, "TagsServlet: File '" + path + "' not found")
        else
            {
            val s = Tags.processStream(ins.get)
            if (s.isEmpty)
                resp.sendError(500, "TagsServlet: unable to process")
            else
                resp + s.get
            }        
        }


    override def doGet(req: Request, resp: Response) =
        {
        val path = req.servletPath  //because the whole path is being used
        showTemplate(path, resp)
        }
}

