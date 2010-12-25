/**
 * Tiny web templating package
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (C) 2010 Bob Jamison
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

/**
 * Here we have our set of default tags
 * 
 * Tags keeps a Map of all tags, and allows external packages
 * to register new tags at startup time
 */    

trait Tag extends pedro.util.Logged
{
    val name : String

    private val hex = "0123456789abcdef".toCharArray

    def xmlStr(ins: String)  =
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

    def apply(attrs: Map[String,String], indent: String) : String
}



//#############################################################################
//# D E F A U L T    T A G S
//#############################################################################

case object DateTag extends Tag
{
    val name = "date"

    def apply(attrs: Map[String,String], indent: String) : String =
        {
        val date = new java.util.Date
        xmlStr(date.toString)
        }
}





//#############################################################################
//# R E G I S T R Y
//#############################################################################


/**
 * Our tag connection point and registry
 */ 
class Tags extends pedro.util.Logged
{
    val namespace = "pedro.tags"

    /**
     * We want to allow external classfiles and jars
     * to add tags to this list at startup time
     */              
    var tags = Map[String, Tag](
        DateTag.name -> DateTag
        )


    def registerTag(tag: Tag) =
        tags += tag.name -> tag

    def registerTags(newtags: Map[String, Tag]) =
        tags ++= newtags

    def apply(name: String, attrs: Map[String, String], col: Int) : String =
        {
        val tag = tags.get(name)
        if (tag.isDefined)
            {
            val spaces = " "*col
            tag.get(attrs, spaces)//construct, then use
            }
        else
            {
            error("tag '" + name + "' not found")
            "<p>error: tag'" + name + "' not found</p>\n"
            }
        }

    def processStream(ins: java.io.InputStream) : Option[String] =
        (new Templater(this)).processStream(ins)
        
}


/**
 * Parent object for Tags and TemplateServlets
 */ 
object Tags extends Tags
{
    
}
