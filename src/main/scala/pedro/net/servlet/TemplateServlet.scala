
package pedro.net.servlet


class TagsServlet extends Servlet
{
    override def doGet(in: Request, out: Response) =
        {
        val path = in.servletPath  //because the whole path is being used
        //println("path: " + path)
        val ins = getServletContext.getResourceAsStream(path)
        if (ins == null)
            out.sendError(404, "TagsServlet: File '" + path + "' not found")
        else
            out + Tags.processStream(ins).get
        }
}

