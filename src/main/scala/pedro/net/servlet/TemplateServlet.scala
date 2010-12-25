
package pedro.net.servlet


class TemplateServlet extends Servlet
{
    override def doGet(in: Request, out: Response) =
        {
        val path = in.servletPath  //because the whole path is being used
        //println("path: " + path)
        val ins = Option(getServletContext.getResourceAsStream(path))
        if (ins.isEmpty)
            out.sendError(404, "TagsServlet: File '" + path + "' not found")
        else
            {
            val s = Tags.processStream(ins.get)
            if (s.isEmpty)
                out.sendError(500, "TagsServlet: unable to process")
            else
                out + s.get
            }
        }
}

