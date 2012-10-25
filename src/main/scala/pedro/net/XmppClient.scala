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
    
    
    def read : Int
    
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



//########################################################################
//### class XmppClient
//########################################################################
class XmppClient(
    val host: String,
    val port: Int,    
    val user: String,
    val domain: String,
    val resource: String,
    val pass: String,
    val debug: Boolean, 
    val callbacks: PartialFunction[XmppEvent, Unit] = XmppNullPF
    )
{
    val jid = user + "@" + domain + "/" + resource
    
    def trace(msg: String) =
        println("XmppClient: " + msg)

    def status(msg: String) =
        post(XmppStatus(msg))
        
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
        
    var idCtr = 0
    def id =
        {
        idCtr += 1
        "id_" + idCtr.toString
        }
        
    private val conn = new XmppTcpConnection(host, port)
    
    private def readStreamHead : Option[Element] =
        {
        val pushParser = new XmlPush(1, "</stream:stream>", "", true)
        var res : Option[Element] = None
        while (res.isEmpty)
            {
            var ch = conn.read
            if (ch < 0)
                return None
            res = pushParser.append(ch)
            }
        res            
        }
        
    private def readStanza : Option[Element] =
        {
        val pushParser = new XmlPush(addNS = "xmlns:stream='http://etherx.jabber.org/streams'")
        var res : Option[Element] = None
        while (res.isEmpty)
            {
            var ch = conn.read
            if (ch < 0)
                return None
            res = pushParser.append(ch)
            }
        res            
        }
        
    private def toHex(arr: Array[Byte]) : String =
        arr.map(b=> "%02x".format(b & 255)).mkString
        
    private def md5hash(arr: Array[Byte]) : Array[Byte] =
        java.security.MessageDigest.getInstance("MD5").digest(arr)
        
    private def toBase64(arr: Array[Byte]) : String =
        javax.xml.bind.DatatypeConverter.printBase64Binary(arr)
    
    private def fromBase64(b64: String) : Array[Byte] =
        javax.xml.bind.DatatypeConverter.parseBase64Binary(b64)
        
    private def parseEncoded(b64: String) : Map[String, String] =
        {
        val raw = new String(fromBase64(b64))
        //trace("raw: " + raw)
        val pairs = raw.split(",")
        val tuples = pairs.map(s =>
            {
            val kv = s.trim.split("=")
            var v = kv(1).trim
            if (v.startsWith("\""))
                v = v.substring(1)
            if (v.endsWith("\""))
                v = v.substring(0, v.size-1)
            (kv(0).trim, v)
            })
        tuples.toMap.withDefaultValue("")
        }
        
    private def md5auth(challenge: String) : String =
        {
        val enc = "UTF-8"
        val props = parseEncoded(challenge)
        trace("props: " + props)  
        val nonce = props("nonce")
        val realm = props("realm")
        val qop   = props("qop")
        val nc = "00000001"
        val cnonce = "abjqwerty"
        val x = user + ":" + realm + ":" + pass
        val y = md5hash(x.getBytes(enc))
        val a1 = y ++ ((":" + nonce + ":" + cnonce).getBytes(enc))
        val a2 = ("AUTHENTICATE:xmpp/" + realm).getBytes(enc)
        val kd = toHex(md5hash(a1)) + ":" + nonce + ":" + nc +
             ":" + cnonce + ":" + qop + ":" + toHex(md5hash(a2))
        val z = toHex(md5hash(kd.getBytes(enc)))

        val rs = ("username=\"%s\",realm=\"%s\",nonce=\"%s\",cnonce=\"%s\","+
            "nc=%s,qop=%s,digest-uri=\"xmpp/%s\",response=%s,charset=utf-8").format(
            user,realm,nonce,cnonce,nc,qop,realm,z)
        trace("rs: " + rs)
        val tag = "<response xmlns='urn:ietf:params:xml:ns:xmpp-sasl'>" +
               toBase64(rs.getBytes(enc)) + "</response>"
        trace("resp: " + tag)
        tag
        }
        
    class Receiver extends Thread
        {
        var cont = false
        override def run =
            {
            cont = true
            while (cont)
                {
                var elem = readStanza
                if (elem.isDefined)
                    trace("rcvr: " + elem)
                }
            }
            
        def abort =
            cont = false
        }
        
    private var receiver = new Receiver
    
    private def doHandshake : Boolean =
        {
        val startmsg =
                "<stream:stream to='" + domain + "' xmlns='jabber:client' " +
                "xmlns:stream='http://etherx.jabber.org/streams' "+
                "version='1.0'>"
        conn.write(startmsg)  
        trace("msg: " + startmsg)
        var elem = readStreamHead
        trace("elem: " + elem) 
        elem = readStanza
        trace("elem: " + elem) 
        val starttls = (elem.get \\ "starttls").size > 0
        trace("starttls: " + starttls)
        var mechanisms = (elem.get \\ "mechanism").map(_.value)
        trace("mech: " + mechanisms)
        if (starttls)
            {
            conn.write("<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>")
            elem = readStanza
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
                elem = readStreamHead
                trace("elem: " + elem) 
                elem = readStanza
                trace("elem: " + elem) 
                mechanisms = (elem.get \\ "mechanism").map(_.value)
                trace("mech: " + mechanisms)
                val authmsg = "<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='DIGEST-MD5'/>"
                conn.write(authmsg)
                elem = readStanza
                trace("elem: " + elem)
                val encoded = elem.get.value
                trace("enc: " + encoded)  
                conn.write(md5auth(encoded))
                elem = readStanza
                trace("elem: " + elem)
                conn.write(startmsg)
                elem = readStreamHead
                trace("elem: " + elem)
                elem = readStanza
                trace("elem: " + elem)
                conn.write("<iq type='set' id='" + id + "'>" +
                     "<bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'>" +
                     "<resource>" + resource + "</resource></bind></iq>")
                elem = readStanza
                trace("elem: " + elem)
                conn.write("<iq type='set' id='" + id + "'>" +
                     "<session xmlns='urn:ietf:params:xml:ns:xmpp-session'/></iq>")
                elem = readStanza
                trace("elem: " + elem)
                
                receiver.start
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


object XmppNullPF extends PartialFunction[XmppEvent, Unit]
{
    def isDefinedAt(v: XmppEvent) = false
    def apply(v: XmppEvent) : Unit = {}
}



object XmppClient
{
    val jidRegex = "^(?:([^@/<>'\"]+)@)?([^@/<>'\"]+)(?:/([^<>'\"]*))?$".r


    def apply(host: String = "host",
              port: Int = 5222, 
              jid: String = "user@example.com/hello",
              pass: String = "nopass",
              debug: Boolean = false,
              callbacks: PartialFunction[XmppEvent, Unit] = XmppNullPF
              ) : XmppClient =
        {
        val jidRegex(user, domain, resource) = jid
        new XmppClient(host, port, user, domain, resource, pass, debug, callbacks)
        }

}



object XmppClientTest
{

    def doTest =
        {
        val cli = XmppClient(debug=true, host="129-7-67-40.dhcp.uh.edu", jid="ishmal@129-7-67-40.dhcp.uh.edu/scala", pass="not known")
        cli.connect
        }

    def main(argv: Array[String]) : Unit =
        doTest

}



