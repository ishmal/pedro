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

import pedro.data.{XmlPush, Element}
import java.io.{BufferedReader, InputStreamReader, BufferedWriter, OutputStreamWriter}
import java.net.Socket
import java.security.{KeyStore,SecureRandom}
import java.security.cert.X509Certificate
import javax.net.ssl.{HandshakeCompletedEvent,HandshakeCompletedListener}
import javax.net.ssl.{SSLContext,SSLSocket,SSLSocketFactory,TrustManager,TrustManagerFactory,X509TrustManager}

//########################################################################
//### Connections
//########################################################################
trait XmppConnection
{
    val pushParser = new XmlPush
    
    def read : Int
    
    def readStanza(level: Int = 0, suffix: String = "") : Option[Element] =
        {
        var res : Option[Element] = None
        while (res.isEmpty)
            {
            var ch = read
            if (ch < 0)
                return None
            val res = pushParser.append(ch, level, suffix)
            }
        res
        }
}

class XmppTcpConnection(val host: String, val port: Int) extends XmppConnection
{

    private var sock = new java.net.Socket
    private var istr : Option[BufferedReader] = None
    private var ostr : Option[BufferedWriter] = None

    private def err(msg: String) =
        println("XmppTcpConnection err: " + msg)
    private def trace(msg: String) =
        println("XmppTcpConnection: " + msg)
        
    def attachStreams =
        {
        istr = Some(new BufferedReader(new InputStreamReader(sock.getInputStream)))
        ostr = Some(new BufferedWriter(new OutputStreamWriter(sock.getOutputStream)))
        }
    
    def open : Boolean =
        {
        try
            { 
            sock = new java.net.Socket(host, port) 
            attachStreams
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
        sock.close
        istr = None
        ostr = None
        true
        }
        

    def read : Int =
        {
        if (istr.isEmpty)
           -1
        else
            {
            istr.get.read
            }
        }
        
    def write(str: String) : Boolean =
        {
        if (ostr.isEmpty)
            false
        else
            {
            ostr.get.write(str)
            ostr.get.flush
            true
            }
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
    val defaultSocketFactory = SSLSocketFactory.getDefault.asInstanceOf[SSLSocketFactory]
        
    def trySSL(factory: SSLSocketFactory) : Option[Socket] =
        {
        try
            {
            val sslsock = factory.createSocket(sock, sock.getInetAddress().getHostAddress(),
                     sock.getPort(), true).asInstanceOf[SSLSocket]
            sslsock.setUseClientMode(true)
            sslsock.startHandshake
            Some(sslsock)
            }
        catch
            {
            case e: Exception => err("trySSL: " + e)
                None
            }
        }
    def goSSL : Boolean =
        {
        val sslsock = trySSL(customSocketFactory)
        //val sslsock = trySSL(defaultSocketFactory)
        if (sslsock.isEmpty)
            false
        else
            {
            sock = sslsock.get
            attachStreams
            true
            }
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
        val startmsg =
                "<stream:stream to='" + domain + "' xmlns='jabber:client' " +
                "xmlns:stream='http://etherx.jabber.org/streams' "+
                "version='1.0'>"
        conn.write(startmsg)  
        trace("msg: " + startmsg)
        var elem = conn.readStanza(2, "</stream:stream>")
        trace("elem: " + elem) 
        val starttls = (elem.get \\ "starttls").size > 0
        trace("starttls: " + starttls)
        var mechanisms = (elem.get \\ "mechanism").map(_.value)
        trace("mech: " + mechanisms)
        if (starttls)
            {
            conn.write("<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>")
            elem = conn.readStanza()
            trace("elem: " + elem)
            if (elem.get.name == "proceed")
                {
                if (!conn.goSSL)
                    {
                    trace("starttls failed")
                    return false
                    }
                trace("starttls ok")
                trace("msg: " + startmsg)
                conn.write(startmsg)  
                elem = conn.readStanza()
                trace("elem: " + elem) 
                val authmsg = "<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='DIGEST-MD5'/>"
                conn.write(authmsg)
                elem = conn.readStanza()
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
        val cli = new XmppClient(debug=true, host="localhost", jid="user@129-7-67-40.dhcp.uh.edu", pass="pass")
        cli.connect
        }

    def main(argv: Array[String]) : Unit =
        doTest

}



