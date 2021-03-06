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
package pedro.net.xmpp


import akka.actor.{Actor, ActorSystem, Props}

import pedro.data.Element
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
    
    def inputStream : java.io.InputStream
    
}

class XmppTcpConnection(val host: String, val port: Int) extends XmppConnection
{

    private var sock = new java.net.Socket
    private var ostr : Option[BufferedWriter] = None

    private def err(msg: String) =
        println("XmppTcpConnection err: " + msg)
    private def trace(msg: String) =
        println("XmppTcpConnection: " + msg)
        
    def attachStreams =
        {
        ostr = Some(new BufferedWriter(new OutputStreamWriter(sock.getOutputStream)))
        }
    
    def open : Boolean =
        {
        try
            { 
            sock = new java.net.Socket(host, port) 
            sock.setSoTimeout(100) //to allow us to kill reader threads
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
        
    def inputStream = sock.getInputStream
        
    def close : Boolean =
        {
        sock.close
        ostr = None
        true
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
            var done = false
            val sslsock = factory.createSocket(sock, sock.getInetAddress().getHostAddress(),
                     sock.getPort(), true).asInstanceOf[SSLSocket]
            sslsock.setUseClientMode(true)
            sslsock.addHandshakeCompletedListener(new javax.net.ssl.HandshakeCompletedListener
                {
                def handshakeCompleted(evt: javax.net.ssl.HandshakeCompletedEvent)
                    {
                    done = true
                    }
                })
            sslsock.startHandshake
            while (!done)
                {
                Thread.sleep(100)
                }
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
    val debug: Boolean 
    ) extends Actor with pedro.util.Logged
{
    val par = context.parent
    
    val jid = user + "@" + domain + "/" + resource
    
    def status(msg: String) =
        par ! XmppStatus(msg)
                
    
    //########################################################
    //# U T I L I T Y
    //########################################################

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
        
    private def xmlStr(s: String) : String =
        pedro.data.Xml.xmlStr(s)

    //########################################################
    //# A U T H
    //########################################################

    private def authMd5(challenge: String) : String =
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
        tag
        }
        
    private def authPlain : String =
        {
        val clear = "\u0000" + user + "\u0000" + pass
        val tag = "<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' " +
            "mechanism='PLAIN'>" + toBase64(clear.getBytes("UTF-8")) + "</auth>"
        tag
        }
                
    var idCtr = 0
    def id =
        {
        idCtr += 1
        "id_" + idCtr.toString
        }
        
    private val streamStartMsg =
                "<stream:stream to='" + domain + "' xmlns='jabber:client' " +
                "xmlns:stream='http://etherx.jabber.org/streams' "+
                "version='1.0'>"

    private val conn = new XmppTcpConnection(host, port)
    
    def write(msg: String) : Boolean =
        {
        trace("write: " + msg)
        conn.write(msg)
        }
        
    //###############################################
    //# I Q
    //###############################################
    
    private def iqReceive(elem: Element) =
        {
        val from = elem("from")
        val to   = elem("to")
        val typ  = elem("type")
        val isQuery = ((elem \\ "query").size > 0)
        if ((elem \\ "ping").size > 0)
            {
            val msg = "<iq from='" + to + "' to='" + from + "' id='" + id +"' type='result'/>"
            write(msg)
            }
        if ((elem \\ "bind").size > 0)
            {
            val jid = (elem \ "bind" \ "jid").text
            trace("jid: " + jid)
            }
        
        }
    
    //###############################################
    //# P R E S E N C E
    //###############################################
    
    private def presenceReceive(elem: Element) =
        {
        val from = elem("from")
        val to   = elem("to")
        }
    
    //###############################################
    //# M E S S A G E
    //###############################################
    
    private def messageReceive(elem: Element) =
        {
        val from = elem("from")
        val to   = elem("to")
        val body = (elem \ "body").text
        trace("Got message! : " + from + " : " + body)
        if (body.size > 0)
            {
            val msg = "<message to='" + from + "'><body>You said: " + body + "</body></message>"
            write(msg)
            }
        }
        
    def messageSend(to: String, msg: String) =
        {
        val tag = "<message to='" + to + "' type='chat' xml:lang='en'><body>" +
            xmlStr(msg) + "</body></message>"
        write(tag)
        }
    
    //###############################################
    //# E R R O R
    //###############################################
    
    private def errorReceive(elem: Element) =
        {
        val from = elem("from")
        val to   = elem("to")
        }
        
        
    
    trait State
    case object Start       extends State
    case object StartTls    extends State
    case object SslStart    extends State
    case object Auth1       extends State
    case object Auth2       extends State
    case object Session     extends State
    case object Connected   extends State
    
    private var state : State = Start
    
    
    
    class Receiver extends Actor
        {
        val par = context.parent
            
            
        var abortme = false
        class IStream(wrapped: java.io.InputStream) extends java.io.InputStream
            {
            override def read : Int =
                {
                var cont = true
                while (cont)
                    {
                    try
                        {
                        return wrapped.read
                        }
                    catch
                        {
                        case e: java.net.SocketTimeoutException =>
                            if (abortme)
                                 cont = false
                        }
                    }
                 -1
                 }
            }
 
        val parser = new XmppParser
            {
            override def process(elem: Element) : Boolean =
                {
                //println("ie: " + elem)
                if (stack.size > 0 && stack.top.name == "stream")
                    {
                    par ! elem
                    true
                    }
                else
                    false
                }
            }
            
        class IThread(name:String) extends Thread(name)
            {
            override def run =
                {
                abortme = false
                println("started: " + name)
                parser.parse(new IStream(conn.inputStream))
                println("aborted: " + name)
                }
            }
            
        var thread = new IThread("rcvr:"+state)
            
        def receive =
            {
                
            case "startup" =>
                {
                val thread = new IThread("rcvr:"+state)
                thread.start
                }

            case "preparefortls" =>
                {
                println("received restart");
                parser.abort
                abortme = true
                thread.join
                println("thread done")
                par ! "readyfortls"
                }

            case "shutdown" =>
                {
                println("received shutdown");
                parser.abort
                abortme = true
                thread.join
                println("thread done")
                }

            }            
            
        }//Receiver


    
    def receive =
        {
        case elem: Element   => handleElem(elem)
        case "readyfortls" =>
            if (!conn.goSSL)
                {
                error("Could not perform STARTTLS")
                }
            receiver ! "startup"
            trace("starttls ok")
            write(streamStartMsg)
            state = SslStart

        case "connect" =>
            state = Start
            if (!conn.open)
                {
                //false
                }
            else
                {
                receiver ! "startup"
                write(streamStartMsg)
                }
        case "disconnect" =>
            state = Start
            conn.close
            receiver ! "shutdown"

        }
        


    def handleElem(elem: Element) : Boolean =
        {
        println("state: " + state + "  elem: " + elem)
        state match
            {
            case Start => 
                val hastls = (elem \\ "starttls").size > 0
                if (hastls)
                    {
                    state = StartTls
                    write("<starttls xmlns='urn:ietf:params:xml:ns:xmpp-tls'/>")
                    }
            case StartTls =>
                if (elem.name == "proceed")
                    {
                    receiver ! "preparefortls"
                    }
            case SslStart =>
                if (elem.name == "features")
                    {
                    val mechs = (elem \\ "mechanism").map(_.value).toSet
                    if (mechs("DIGEST-MD5"))
                        {
                        write("<auth xmlns='urn:ietf:params:xml:ns:xmpp-sasl' mechanism='DIGEST-MD5'/>")
                        }
                    else if (mechs("PLAIN"))
                        {
                        write(authPlain)
                        }
                    else
                        {
                        //error!
                        }
                    state = Auth1
                    }
            case Auth1 =>
                if (elem.name == "success")
                    {
                    state = Auth2
                    receive(elem)
                    }
                else if (elem.name == "challenge")
                    {
                    write(authMd5(elem.value))
                    state = Auth2
                    }
            case Auth2 =>
                if (elem.name == "success")
                    {
                    write(streamStartMsg)
                    state = Session
                    }
            case Session =>
                    if (elem.name == "features")
                        {
                        if ((elem \\ "bind").size > 0)
                            {
                            write("<iq type='set' id='" + id + "'>" +
                                "<bind xmlns='urn:ietf:params:xml:ns:xmpp-bind'>" +
                                "<resource>" + resource + "</resource></bind></iq>")
                            }
                        if ((elem \\ "session").size > 0)
                            {
                            write("<iq type='set' id='" + id + "'>" +
                                "<session xmlns='urn:ietf:params:xml:ns:xmpp-session'/></iq>")
                            }
                        //startup things
                        write("<presence/>")
                        write("<iq id='" + id + "' type='get'><query xmlns='jabber:iq:roster'/></iq>")
                        state = Connected   
                        }
            case Connected =>
                elem.name match
                    {
                    case "iq"       => iqReceive(elem)
                    case "message"  => messageReceive(elem)
                    case "error"    => errorReceive(elem)
                    case "presence" => presenceReceive(elem)
                    }
            }
        true
        }
        
    private val receiver = context.actorOf(Props(new Receiver),  name = "receiver")
    
        
    override def postStop =
        {
        println("POST")
        receiver ! "shutdown"
        }

}





class XmppBot(host: String="localhost", port: Int = 5222, jid: String = "user@localhost/scala",
     pass: String = "pass") extends Actor with pedro.util.Logged
{
    val jidRegex = "^(?:([^@/<>'\"]+)@)?([^@/<>'\"]+)(?:/([^<>'\"]*))?$".r
    val jidRegex(user, domain, resource) = jid
    
    val cli = context.actorOf(Props(
           new XmppClient(host, port, user, domain, resource, pass, true)),
             "xmppClient")
             
    def receive =
        {
        case "connect" => cli ! "connect"
        }

}



object XmppClientTest
{

    def doTest =
        {
        /**/
        val host = "129-7-67-40.dhcp.uh.edu"
        val jid = "ishmal@129-7-67-40.dhcp.uh.edu/scala"
        val pass = "password"
        /**/
        val bot = ActorSystem().actorOf(Props(new XmppBot(host=host, jid=jid, pass=pass)))
        bot ! "connect"
        }

    def main(argv: Array[String]) : Unit =
        doTest

}



