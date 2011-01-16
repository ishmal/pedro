/**
 * Limited AMQP 1.0 server.  Intended solely for testing the client
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



package pedro.net.amqp



import scala.actors.Actor
import scala.actors.Actor._


import java.security.cert.X509Certificate


class AmqpServer(port: Int = 5672) extends pedro.util.Logged
{
    
    private lazy val factory : Option[javax.net.ssl.SSLSocketFactory] =
        {        
        java.security.Security.addProvider(new com.sun.net.ssl.internal.ssl.Provider)
        val password = "pedro1".toCharArray
        val ks = java.security.KeyStore.getInstance(java.security.KeyStore.getDefaultType)
        try
            {
            val fis = getClass.getResourceAsStream("amqp.keystore")
            ks.load(fis, password);
            } 
        catch
            {
            case e: Exception => error("retriving keystore: " + e ) ; None
            }
        val permissiveManager = new javax.net.ssl.X509TrustManager
            {
            def getAcceptedIssuers = Array[X509Certificate]()
            def checkClientTrusted(arg0: Array[X509Certificate], arg1: String) = {}
            def checkServerTrusted(arg0: Array[X509Certificate], arg1: String) = {}
            }
        val keyFactory = javax.net.ssl.KeyManagerFactory.getInstance(
            javax.net.ssl.KeyManagerFactory.getDefaultAlgorithm)
        keyFactory.init(ks, password)
        val sslcontext = javax.net.ssl.SSLContext.getInstance( "TLS" )
        sslcontext.init( keyFactory.getKeyManagers,
                Array[javax.net.ssl.TrustManager](permissiveManager),
                new java.security.SecureRandom )
        Some(sslcontext.getSocketFactory.asInstanceOf[javax.net.ssl.SSLSocketFactory])
        }



    class HandlerMessage
    case object Abort extends HandlerMessage

    class Handler(socket: java.net.Socket) extends Actor
    {
        var sock = socket
        

        def goTLS : Boolean =
            {
            try
                {
                if (factory.isEmpty)
                    return false
                val newSock = factory.get.createSocket(sock, 
                    sock.getInetAddress.getHostAddress, sock.getPort, true)
                sock = newSock
                true
                }
            catch
                {
                case e: Exception => error("Could not start SSL: " + e) ; false
                }
            }
        
        def write(b: Array[Byte]) : Boolean =
            {
            try
                {
                val out = sock.getOutputStream
                out.write(b)
                out.flush
                true
                }
            catch
                {
                case e: Exception => error("write: " + e) ; false
                }
            }

        var keepHandling = true
        
        val bufsize = 1024
        val recvBuf = Array.ofDim[Byte](bufsize)
        
        def syncRead : Option[Array[Byte]] = 
            {
            try
                {
                val buf = scala.collection.mutable.ListBuffer[Byte]()
                var len = sock.getInputStream.read(recvBuf)
                buf ++= recvBuf.view(0,len)
                while (len == 1024)
                    {                
                    len = sock.getInputStream.read(recvBuf)
                    buf ++= recvBuf.view(0,len)
                    }
                Some(buf.toArray)
                }
            catch
                {
                case e: Exception => error("syncRead: " + e)
                    None
                }
            }
        
        /**
         * This startup transaction with the client is the only
         * sync part.  Everything else is async
         */                          
        def handshake : Boolean =
            {
            var inb = syncRead
            if (inb.isEmpty)
                return false
            trace("server received:" + inb.get.mkString("[", ",","]") + "  wanted " + Amqp.tlsProto.mkString("[", ",","]"))
            if (inb.get.sameElements(Amqp.tlsProto))
                {
                trace("here")
                write(Amqp.tlsProto)
                trace("Server: client requested TLS")
                if (!goTLS)
                    false
                trace("server has tls")
                }
            true
            }
        
        def receiveFrame : Boolean =
            {
            true
            }

        def act =
            {
            handlers += this
            if (handshake)
                {
                while (keepHandling)
                    {
                    if (!receiveFrame)
                        keepHandling = false
                    else
                        {
                        receiveWithin(1000)
                            {
                            case Abort => keepHandling = false
                            }
                        }
                    }   
                }           
            sock.close
            handlers -= this
            }
        
    }



    
    var ssock : Option[java.net.ServerSocket] = None
    
    val handlers = scala.collection.mutable.Set[Handler]()

    var keepListening = true

    def listen =
        {
        keepListening = true
        while (keepListening && ssock.isDefined)
            {
            try
                {
                val sock = ssock.get.accept
                (new Handler(sock)).start
                }
            catch
                {
                case e: java.net.SocketTimeoutException => // this one is ok
                case e: Exception => error("listen: " + e)
                    keepListening = false
                }
            } 
        
        }


    def start : Boolean =
        {
        try
            {
            val sock = new java.net.ServerSocket(port)
            sock.setSoTimeout(1000)
            ssock = Some(sock)
            actor(listen)
            true
            }
        catch
            {                
            case e: Exception => error("listen: " + e)
                false
            }
        }

    def stop : Boolean =
        {
        handlers.foreach(h=> h!Abort)
        keepListening = false
        if (ssock.isDefined)
            ssock.get.close
        ssock = None
        true
        }

}