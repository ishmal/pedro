/**
 * Tiny limited capability AMQP client
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

import java.security.cert.X509Certificate

import java.net.{InetAddress,Socket}




class TcpConnection(host: String = "localhost", port: Int = 5672,
          username:String = "", password: String = "")
               extends Connection with pedro.util.Logged
{
    var sock : Option[java.net.Socket] = None
    
    def isConnected =
        sock.isDefined && sock.get.isConnected

    def connect =
        {
        disconnect
        try
            {
            val s = new java.net.Socket(host, port)
            sock = Some(s)
            true
            }
        catch
            {
            case e: Exception => error("connect: " + e)
                false
            }
        }

    def disconnect =
        {
        if (isConnected)
            sock.get.close
        sock = None
        true
        }
    
    val inbuf = Array.ofDim[Byte](4096)
    
    def read : Option[Array[Byte]] =
        {
        if (!isConnected)
            {
            error("connection closed")
            None
            }
        else
            {
            try
                {
                val ins = sock.get.getInputStream
                val arr = Array.ofDim[Byte](1024)
                val buf = new scala.collection.mutable.ListBuffer[Byte]()
                var len = ins.read(arr)
                buf ++= arr.view(0,len)
                while (len == 1024)
                    {
                    len = ins.read(arr)
                    buf ++= arr.view(0,len)
                    }
                Some(buf.toArray)
                }
            catch
                {
                case e : Exception => error("read: " + e)
                    None
                }
            }
        }

    def write(arr: Array[Byte]) =
        {
        if (!isConnected)
            {
            error("connection closed")
            false
            }
        else
            {
            try
                {
                val out = sock.get.getOutputStream
                out.write(arr)
                out.flush
                true
                }
            catch
                {
                case e : Exception => error("write: " + e)
                    false
                }
            }
        }

    private lazy val sslFactory =
        {
        java.security.Security.addProvider(new com.sun.net.ssl.internal.ssl.Provider)
        val permissiveManager = new javax.net.ssl.X509TrustManager
            {
            def getAcceptedIssuers = Array[X509Certificate]()
            def checkClientTrusted(arg0: Array[X509Certificate], arg1: String) = {}
            def checkServerTrusted(arg0: Array[X509Certificate], arg1: String) = {}
            }
        val sslcontext = javax.net.ssl.SSLContext.getInstance( "TLS" )
        sslcontext.init( null, // No KeyManager required
                Array[javax.net.ssl.TrustManager](permissiveManager),
                new java.security.SecureRandom )
        sslcontext.getSocketFactory.asInstanceOf[javax.net.ssl.SSLSocketFactory]
        } 

    def goTLS : Boolean =
        {
        try
            {
            val newSock = sslFactory.createSocket(sock.get, host, port, true)
            sock = Some(newSock)
            true
            }
        catch
            {
            case e: Exception => error("Could not start SSL: " + e) ; false
            }
        }


}


/**
 * Placeholder for our upcoming AMQP 1.0 client
 * TODO:  implement.  should take only a few minutes 
 */ 
class AmqpClient(host: String = "localhost", port: Int = 5672,
          username:String = "", password: String = "")
           extends Container with pedro.util.Logged
{
    

    val connection = new TcpConnection(host, port)

    def connect : Boolean =
        {
        if (!connection.connect)
            return false
        connection.write(Amqp.tlsProto)
        trace("a")
        var res = connection.read
        trace("b")
        if (!res.isDefined)
            return false
        trace("received from server: " + res.get.mkString("[",",","]"))
        if (res.get.sameElements(Amqp.tlsProto))
            {
            trace("### Client TLS Accepted!")
            if (!connection.goTLS)
                false
            trace("client has tls")
            }
        true
        }

}


