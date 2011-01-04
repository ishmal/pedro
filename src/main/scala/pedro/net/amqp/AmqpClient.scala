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
    
    def read : Option[Array[Byte]] =
        {
        None
        }

    def write(arr: Array[Byte]) =
        {
        if (!isConnected)
            {
            error("connedtion closed")
            false
            }
        else
            {
            try
                {
                sock.get.getOutputStream.write(arr)
                true
                }
            catch
                {
                case e : Exception => error("write: " + e)
                    false
                }
            }
        }
}


/**
 * Placeholder for our upcoming AMQP 1.0 client
 * TODO:  implement.  should take only a few minutes 
 */ 
class AmqpClient(host: String = "localhost", port: Int = 5672,
          username:String = "", password: String = "") extends Container
{
    val connection = new TcpConnection(host, port)

    def connect : Boolean =
        {
        if (!connection.connect)
            return false
        true
        }

}


