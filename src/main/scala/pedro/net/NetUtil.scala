/**
 *  This is a simple implementation of OAuth for Scala.
 *
 *  Author: Bob Jamison
 *  Copyright, 2011
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

package pedro.net

/**
 * An assortment of utilities that help with common tasks for
 * net apps.
 */ 
object NetUtil extends pedro.util.Logged
{

    /**
     * Convert a Map of name->value strings into a single URL-encoded string
     */
    def paramStr(mp: Map[String, String]) : String =
        mp.toList.sortWith((a,b) => a._1 < b._1).
        map(e => encode(e._1)+"="+encode(e._2)).mkString("&")
    
    /**
     * Perform an HTTP POST operation
     */
    def post(urls: String, 
             params: Map[String, String] = Map(),
             props: Map[String, String] = Map()) : Option[String] =
        {
        val outs = paramStr(params).getBytes
        val url = new java.net.URL(urls)
        val conn = url.openConnection.asInstanceOf[java.net.HttpURLConnection]
        try
            {
            conn.setRequestMethod("POST")
            conn.setDoOutput(true)
            conn.setRequestProperty("Content-Length", outs.length.toString)
            props.foreach(e => conn.setRequestProperty(e._1, e._2))
            conn.getOutputStream.write(outs)
            conn.getOutputStream.close
            Some(read(conn.getInputStream))
            }
        catch
            {
            case e:Exception => error("post: " + e)
                error("post:" + read(conn.getErrorStream))
            None
            }
        }

    /**
     * Perform an HTTP POST operation.  Parse the result into a map of name->value pairs
     */
    def postv(urls: String, 
             params: Map[String, String] = Map(),
             props: Map[String, String] = Map()) : Option[Map[String,String]] =
        {
        val res = post(urls, params, props)
        res.flatMap(s=>Some(parseValues(s)))
        }

    /**
     * Perform an HTTP GET operation
     */
    def get(baseUrl: String, 
             params: Map[String, String] = Map(),
             props: Map[String, String] = Map()) : Option[String] =
        {
        val urls = if (params.size == 0) baseUrl else baseUrl + "?" + paramStr(params)
        val url = new java.net.URL(urls)
        val conn = url.openConnection.asInstanceOf[java.net.HttpURLConnection]
        try
            {
            props.foreach(e => conn.setRequestProperty(e._1, e._2))
            Some(read(conn.getInputStream))
            }
        catch
            {
            case e:Exception => error("get: " + e)
                error("get:" + read(conn.getErrorStream))
            None
            }
        }


    /**
     * Perform an HTTP GET operation.  Parse the result into a map of name->value pairs
     */
    def getv(urls: String, 
             params: Map[String, String] = Map(),
             props: Map[String, String] = Map()) : Option[Map[String,String]] =
        {
        val res = get(urls, params, props)
        res.flatMap(s=>Some(parseValues(s)))
        }


    /**
     * Parse a query string of the form ?name1=value1&name2=value2&  ...etc
     * into a Map of name->value strings
     */
    def parseValues(str: String) : Map[String, String] =
        {
        trace("parseValues:" + str)
        val vals = scala.collection.mutable.Map[String, String]()
        str.split("&").foreach(pair=>
            {
            val p = pair.split("=")
            if (p.size>=2)        // name=value
                vals += p(0) -> p(1)
            else if (p.size>=1)   // name=  or just name
                vals += p(0) -> ""
            })
        vals.toMap
        }

    private val hex = "0123456789ABCDEF".toCharArray //must be uppers

    /**
     * Encode a string into RFC 3986 format, with escapes when needed
     */
    def encode(str: String) : String =
        {
        val buf = new StringBuilder
        str.foreach(ch =>
            {
            if (ch.isLetterOrDigit || ch == '-' || ch == '.' || ch == '_' || ch == '~')
                buf.append(ch)
            else
                buf.append("%").append(hex((ch>>4)&0xf)).append(hex(ch&0xf))
            })
        buf.toString
        }

    /**
     * Convert an array of bytes into Base-64 format
     * Note: Uses the runtime's builtin base64, cleverly hidden away
     */
    def base64(plain: Array[Byte]) : String =
        javax.xml.bind.DatatypeConverter.printBase64Binary(plain)


    /**
     * Convert a string into Base-64 format
     */
    def base64(plain: String) : String =
        base64(plain.getBytes)

    //read an input stream into a string
    private def read(ins: java.io.InputStream) : String =
        scala.io.Source.fromInputStream(ins).mkString


}

