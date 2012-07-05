/**
 *  This is a simple implementation of OAuth for Scala.
 *
 *  Author: Bob Jamison
 *  Copyright, 2012
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


package pedro.net.oauth

import java.io.InputStream
import pedro.data.{Data}
import pedro.net.servlet.{Servlet,Request,Response}


case class App(
    val id       : String = "",
    val org      : String = "",
    val desc     : String = "",
    val url      : String = "",
    val userid   : String = "",
    val password : String = ""
) extends Data
{}


trait OAuthValidator 
{

    /**
     * Check that the given message from the given accessor is valid.
     * 
     * @throws IOException
     *             the message couldn't be read.
     * @throws URISyntaxException
     *             the message URL is invalid.
     */
    def validateMessage(message: OAuthMessage,
            accessor: OAuthAccessor) : Boolean =
        {
        true
        }

}


class RegistryServlet extends Servlet
{
    /**
    override def doGet(req: Request, resp: Response) =
        {
        resp + """
            <html>
            <head>
            </head>
            <body>
        """

        val cmd = req("cmd")
        if (cmd == "show")
            {
            val id = req("id")
            val res = Db.get(app, id)
            if (res.isEmpty)
                {
                
                }
            else
                {
                
                }
            }
        else if (cmd == "form")
            {
            resp + """
            <form>
            </form>
            """"
            }
        else
            {
            val res = Db.list(app)
            res.get.foreach(app=>
                {
                })
            }
        resp + """
            </body>
            </html>
        """
        }
    */
    override def doPost(req: Request, resp: Response) =
        {
        
        }

    override def doPut(req: Request, resp: Response) =
        {
        
        }

    override def doDelete(req: Request, resp: Response) =
        {
        
        }

}


