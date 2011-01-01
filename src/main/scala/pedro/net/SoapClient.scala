/**
 * Minimalist SOAP client
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

package pedro.net

import scala.xml.{Elem,XML}


class SoapClient
{

    private def error(msg: String) =
        {
        println("SoapClient error: " + msg)
        }

    /**
     * Call this function with the SOAP content part.  Will return the complete
     * serialized message
     */              
    def wrap(xml: Elem) : String =
        {
        val buf = new StringBuilder
        buf.append("<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"no\"?>\n")
        buf.append("<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://schemas.xmlsoap.org/soap/envelope/\">\n")
        buf.append("<SOAP-ENV:Body>\n")
        buf.append(xml.toString)
        buf.append("\n</SOAP-ENV:Body>\n")
        buf.append("</SOAP-ENV:Envelope>\n")
        buf.toString
        }


    /**
     * Sends the SOAP message, and returns (if successful) the reply.
     */              
    def sendMessage(host: String, req: Elem) : Option[Elem] =
        {
        val url = new java.net.URL(host)
        val outs = wrap(req).getBytes
        val conn = url.openConnection.asInstanceOf[java.net.HttpURLConnection]
        try
            {
            conn.setRequestMethod("POST")
            conn.setDoOutput(true)
            conn.setRequestProperty("Content-Length", outs.length.toString)
            conn.setRequestProperty("Content-Type", "text/xml")
            conn.getOutputStream.write(outs)
            conn.getOutputStream.close
            Some(XML.load(conn.getInputStream))
            }
        catch
            {
            case e:Exception => error("sendMessage: " + e)
                error("sendMessage:" + scala.io.Source.fromInputStream(conn.getErrorStream).mkString)
            None
            }
        }
      
}

object SoapTest
{
    def doTest1 =
        {
        val host = "https://apitest.authorize.net/soap/v1/Service.asmx"
        val req  = <IsAlive xmlns="https://api.authorize.net/soap/v1/"/>
        val cli = new SoapClient
        println("##### request:\n" + cli.wrap(req))
        val resp = cli.sendMessage(host, req)
        if (resp.isDefined)
            {
            println("##### response:\n" + resp.get.toString)
            }
        }

    def doTest2 =
        {
        val host = "http://ws.cdyne.com/WeatherWS/Weather.asmx"
        val req  = <GetCityForecastByZIP xmlns="http://ws.cdyne.com/WeatherWS/">
                     <ZIP>77058</ZIP>
                    </GetCityForecastByZIP>
        val cli = new SoapClient
        println("##### request:\n" + cli.wrap(req))
        val resp = cli.sendMessage(host, req)
        if (resp.isDefined)
            {
            println("##### response:\n")
            (resp.get  \\ "Forecast").foreach(elem=>
                {
                println("#########\n" + elem.toString)  
                })
               
            }
        }
    

    def main(argv: Array[String]) =
        doTest2
}