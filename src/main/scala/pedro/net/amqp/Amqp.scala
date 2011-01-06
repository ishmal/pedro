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



trait Frame
{
    val frameType : Int = 0 //AMQP frame
    
    val channel : Short = 0
    
    private def toBytes(i: Long) =
        Array( ((i>>56)&0xff).toByte, ((i>>48)&0xff).toByte,
               ((i>>40)&0xff).toByte, ((i>>32)&0xff).toByte,
               ((i>>24)&0xff).toByte, ((i>>16)&0xff).toByte,
               ((i>> 8)&0xff).toByte, ((i    )&0xff).toByte)

    private def toBytes(i: Int) =
        Array( ((i>>24)&0xff).toByte, ((i>>16)&0xff).toByte,
               ((i>> 8)&0xff).toByte, ((i    )&0xff).toByte)

    private def toBytes(i: Short) =
        Array( ((i>> 8)&0xff).toByte, (i&0xff).toByte )

    def bytes : Array[Byte] =
        {
        val bodyArr = body
        val extArr  = extendedHeader
        val size    = 8 + extArr.size + bodyArr.size
        val doff    = extArr.size / 4 + 2
        toBytes(size) ++ Array(doff.toByte, frameType.toByte) ++
             toBytes(channel) ++ extArr ++ bodyArr
        }
    
    /**
     * override this if your Frame type needs it
     */         
    def extendedHeader : Array[Byte] =
        Array()

    /**
     * define this serialization for each frame type
     */         
    def body : Array[Byte] =
        Array()
}


trait Link
{
    val name    : String   
    val source  : String   
    val target  : String   
    val timeout : Int      
}

trait Session
{
    val name : String   
}

trait Node
{
    
}

trait Producer extends Node
trait Consumer extends Node
trait Queue extends Node

trait Connection
{
    def isConnected : Boolean
    
    def connect : Boolean
    
    def disconnect : Boolean
    
    def read : Option[Array[Byte]]
    
    def write(arr: Array[Byte]) : Boolean
    
    def write(f: Frame) : Boolean =
        write(f.bytes)
}

trait Container
{
    
    val connection : Connection
    
    private val sess = scala.collection.mutable.ListBuffer[Session]()
    def sessions : Seq[Session] =
        sess.toList

    private val prods = scala.collection.mutable.ListBuffer[Producer]()
    def producers : Seq[Producer] =
        prods.toList

    private val cons = scala.collection.mutable.ListBuffer[Consumer]()
    def consumers : Seq[Consumer] =
        cons.toList

    private val ques = scala.collection.mutable.ListBuffer[Queue]()
    def queues : Seq[Queue] =
        ques.toList
}



class Encoder
{
    private val baos = new java.io.ByteArrayOutputStream
    private val outs = new java.io.DataOutputStream(baos)
    private def t(v: Int) = outs.write(v & 0xff)
    def bytes = baos.toByteArray


    def encode(value: Any) : Encoder =
        {
        value match
           {
           case None          => t(0x40)
           case true          => t(0x41)
           case false         => t(0x42)
           case v:Byte        => t(0x51) ; outs.writeByte(v)
           case v:Short       => t(0x61) ; outs.writeShort(v)
           case v:Int         => t(0x71) ; outs.writeInt(v)
           case v:Long        => t(0x81) ; outs.writeLong(v)
           case v:Float       => t(0x72) ; outs.writeFloat(v)
           case v:Double      => t(0x82) ; outs.writeDouble(v)
           case v:String      => val b = v.getBytes
                                 if (b.size <256)
                                     { t(0xa1) ; outs.writeByte(b.size) }
                                 else
                                     { t(0xb1) ; outs.writeInt(b.size) }
                                 outs.write(b)
           case v:Map[_,_]    => if (v.size < 128)
                                     { t(0xc1) ; outs.writeByte(v.size*2) }
                                 else
                                     { t(0xd1) ; outs.writeInt(v.size*2) }
                                 v.foreach(item=> {encode(item._1) ; encode(item._2)})
           case v:Array[_]    => if (v.size < 256)
                                     { t(0xe0) ; outs.writeByte(v.size) }
                                 else
                                     { t(0xf0) ; outs.writeInt(v.size) }
                                 v.foreach(encode)
           case v:Iterable[_] => if (v.size < 256)
                                     { t(0xc0) ; outs.writeByte(v.size) }
                                 else
                                     { t(0xd0) ; outs.writeInt(v.size) }
                                 v.foreach(encode)
           case _             => error("encode: type of '" + value.toString + "'' not supported")
           }

        this
        }
}




class Decoder(inBytes: Array[Byte]) extends pedro.util.Logged
{   
    val in = new java.io.DataInputStream(new java.io.ByteArrayInputStream(inBytes))

    def decode : Any =
        {
        val typ = in.readByte
        typ match
            {
            case 0x40 => None
            case 0x41 => true
            case 0x42 => false
            case 0x51 => in.readByte
            case 0x61 => in.readShort
            case 0x71 => in.readInt
            case 0x81 => in.readLong
            case 0x72 => in.readFloat
            case 0x82 => in.readDouble
            case 0xa1 => new String(Array.tabulate[Byte](in.readByte)(_=> in.readByte))
            case 0xb1 => new String(Array.tabulate[Byte](in.readByte)(_=> in.readByte))
            case 0xc1 => Array.tabulate(in.readByte/2)(_=> (decode, decode)).toMap
            case 0xd1 => Array.tabulate(in.readInt/2)(_=> (decode, decode)).toMap
            case 0xe0 => Array.tabulate(in.readByte)(_=>decode)
            case 0xf0 => Array.tabulate(in.readInt)(_=>decode)
            case 0xc0 => List.tabulate(in.readByte)(_=>decode)
            case 0xd0 => List.tabulate(in.readInt)(_=>decode)
            case _    => throw new Exception("decode: type for '" + typ + "' not implemented")
            }
        }
}



object Codec extends pedro.util.Logged
{
    def encode(value: Any) : Option[Array[Byte]] =
        {
        try
            {
            Some((new Encoder).encode(value).bytes)
            }
        catch
            {
            case e: Exception => error("encode: " + e)
                None
            }
        }
         
    def decode(arr: Array[Byte]) : Option[Any] =
        {
        try
            {
            Some((new Decoder(arr)).decode)
            }
        catch
            {
            case e: Exception => error("decode: " + e)
                None
            }
        }    
}

