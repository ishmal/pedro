/**
 * Simple Scala XMPP client lib
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (C) 2012 Bob Jamison
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

import pedro.data.XmlReader
import java.net.Socket
import java.security.SecureRandom
import java.security.cert.X509Certificate
import javax.net.ssl.{HandshakeCompletedEvent,HandshakeCompletedListener}
import javax.net.ssl.{SSLContext,SSLSocket,TrustManager,X509TrustManager}

//########################################################################
//### Connections
//########################################################################
trait XmppConnection

class XmppTcpConnection(val host: String, val port: Int) extends XmppConnection
{

    private var sock = new java.net.Socket
    private var reader : Option[java.io.InputStreamReader] = None
    private var writer : Option[java.io.BufferedWriter] = None
    
    private def err(msg: String) =
        println("XmppTcpConnection err: " + msg)
    private def trace(msg: String) =
        println("XmppTcpConnection: " + msg)
    
    def open : Boolean =
        {
        try
            { 
            sock = new java.net.Socket(host, port) 
            reader = Some(new java.io.InputStreamReader(sock.getInputStream, "UTF-8"))
            writer = Some(new java.io.BufferedWriter(new java.io.OutputStreamWriter(sock.getOutputStream, "UTF-8")))
            true
            }
        catch
            { 
            case e : Exception => 
                err("open: " + e)
                false
            }
        }
        
    def close : Boolean =
        {
        reader = None
        writer = None
        sock.close
        true
        }
        
    def read : Option[String] =
        {
        if (reader.isEmpty)
            None
        else
            {
            val r = reader.get
            while (!r.ready)
                {}
            val buf = new StringBuilder
            while (r.ready)
                buf.append(r.read.toChar)
            Some(buf.toString)
            }
        }
        
    def write(str: String) : Boolean =
        {
        if (writer.isDefined)
            {
            val w = writer.get
            w.write(str)
            //w.newLine
            w.flush
            true
            }
        else
            false
        }

    
    val customSocketFactory =
        {
        val sc = SSLContext.getInstance("TLS")
        val trm = new X509TrustManager
            {
            def checkClientTrusted(chain: Array[X509Certificate], authType: String) = {}
            def checkServerTrusted(chain: Array[X509Certificate], authType: String) = {}
            def getAcceptedIssuers = null
            }

        sc.init(null, Array[TrustManager](trm), new SecureRandom)
        sc.getSocketFactory
        }
        
    def goSSL : Boolean =
        {
        val factory = customSocketFactory
        //val factory = SSLSocketFactory.getDefault.asInstanceOf[SSLSocketFactory]
        val sslsock = factory.createSocket(sock, host, port, true).asInstanceOf[SSLSocket]
        var done = false
        sslsock.addHandshakeCompletedListener(new HandshakeCompletedListener
            {
            def handshakeCompleted(event: HandshakeCompletedEvent) =
               done = true
            })
        sslsock.startHandshake
        while (!done)
            {
            Thread.sleep(100)
            }
        trace("handshake done")
        sock = sslsock
        reader = Some(new java.io.InputStreamReader(sock.getInputStream, "UTF-8"))
        writer = Some(new java.io.BufferedWriter(new java.io.OutputStreamWriter(sock.getOutputStream,"UTF-8")))         
        true
        }
}



//########################################################################
//### class XmppEvent
//########################################################################
trait XmppEvent

case object XmppNone
case class XmppConnected(user:String, host:String) extends XmppEvent
case class XmppDisconnected(user:String, host:String) extends XmppEvent

case class XmppStatus(msg: String) extends XmppEvent


object XmppNullPF extends PartialFunction[XmppEvent, Unit]
{
    def isDefinedAt(v: XmppEvent) = false
    def apply(v: XmppEvent) : Unit = {}
}

class XmppClient(
    val host: String     = "jabber.org",
    val port: Int        = 5222,
    val jid: String      = "user@host",
    val pass: String     = "nopass",
    val resource: String = "",
    val debug: Boolean  = false, 
    val callbacks: PartialFunction[XmppEvent, Unit] = XmppNullPF
    )
{
    
    def trace(msg: String) =
        println("XmppClient: " + msg)

    def status(msg: String) =
        post(XmppStatus(msg))
        
    private val words = jid.split("@")
    
    val user = words(0)
    val domain = words(1)


    private val defaultCallbacks : PartialFunction[XmppEvent, Unit] =
        {
        case XmppConnected(user, host) =>
            trace("connected: " + user + " : " + host)
        case XmppDisconnected(user, host) =>
            trace("disconnected: " + user + " : " + host)
        case XmppStatus(msg) =>
            trace("status:" + msg)
        }

    private val handler = callbacks orElse defaultCallbacks
        
    private def post(evt: XmppEvent) =
        handler(evt)
        
    private val conn = new XmppTcpConnection(host, port)
    
    private def doHandshake : Boolean =
        {
        val startmsg = "<?xml version='1.0'?>" +
                "<stream:stream to='" + domain + "' xmlns='jabber:client' " +
                "xmlns:stream='http://etherx.jabber.org/streams' "+
                "version='1.0'>"
        conn.write(startmsg)  
        trace("msg: " + startmsg)
        var res = conn.read
        trace("res: " + res) 
        var elem = XmlReader.parse(res.get + "</stream:stream>")
        trace("elem: " + elem)
        val starttls = (elem.get \\ "starttls").size > 0
        trace("starttls: " + starttls)
        var mechanisms = (elem.get \\ "mechanism").map(_.value)
        trace("mech: " + mechanisms)
        if (starttls)
            {
            conn.write("<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>")
            val rr = conn.read
            trace("rr: " + rr)
            if (rr.get.indexOf("proceed") >= 0)
                {
                if (!conn.goSSL)
                    {
                    trace("starttls failed")
                    return false
                    }
                trace("starttls ok")
                conn.write(startmsg)  
                trace("msg: " + startmsg)
                res = conn.read
                trace("res: " + res) 
                elem = XmlReader.parse(res.get + "</stream:stream>")
                trace("elem: " + elem)
                mechanisms = (elem.get \\ "mechanism").map(_.value)
                trace("mech: " + mechanisms)
                    
                }
            else
                {
                return false
                }
            }
        true
        }
    
        
        
    def connect : Boolean =
        {
        if (!conn.open)
            {
            false
            }
        else
            {
            doHandshake
            }
        }
        
    def disconnect : Boolean =
        {
        conn.close
        }
    

}





object XmppClientTest
{

    def doTest =
        {
        val cli = new XmppClient(debug=true, host="jabber.org", jid="user@jabber.org", pass="pass")
        cli.connect
        }

    def main(argv: Array[String]) : Unit =
        doTest

}



