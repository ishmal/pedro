/**
 * Minimalist NTP client
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

case class Timestamp(v: Double)
{
    def toBytes(arr: Array[Byte], offset:Int) =
        {
        var tv = v
        for (i <- 0 until 8)
            {
            val pwr = math.pow(2.0, 8.0 * (3-i))
            val fv = (tv/pwr).toInt
            arr(i + offset) = fv.toByte
            tv -= pwr * fv
            }
        }
        
    private lazy val dateFormat = new java.text.SimpleDateFormat("dd-MMM-yyyy HH:mm:ss")
    private lazy val dateFractionFormat = new java.text.DecimalFormat(".000000")
	/**
	 * Returns a timestamp (number of seconds since 00:00 1-Jan-1900) as a
	 * formatted date/time string. 
	 */
	override def toString : String =
	    {
		if (v==0.0)
		   "0"
		else
		    {	
    		// timestamp is relative to 1900, utc is used by Java and is relative to 1970 
    		val utc = v - (2208988800.0)
    		
    		// milliseconds
    		val ms = (utc * 1000.0).toLong
    		
    		// date/time
    		val dateStr = dateFormat.format(new java.util.Date(ms))
    		
    		// fraction
    		val fraction = v - math.floor(v)
    		val fractionStr = dateFractionFormat.format(fraction)
    		
    		dateStr + fractionStr
		    }
	    }
	    
	def -(other: Timestamp) =
	    Timestamp(v - other.v)
	def +(other: Timestamp) =
	    Timestamp(v + other.v)
	def *(scale: Double) =
	    Timestamp(v*scale)
}


object Timestamp
{

	private def toInt(b: Byte) : Int =
		b.toInt & 0xff
		
    def apply(arr: Array[Byte], offset:Int) : Timestamp =
        {
        var v = 0.0
        for (i <- 0 until 8)
            v += math.pow(2.0, 8.0 * (3-i)) * toInt(arr(i + offset))

        new Timestamp(v)
        }
        
    def now : Timestamp =
        {
        new Timestamp((java.lang.System.currentTimeMillis / 1000.0) + 2208988800.0)
        }
}


case class NtpMessage(
	val leapIndicator       : Int         =   0,
	val version             : Int         =   3,
	val mode                : Int         =   3,
	val stratum             : Int         =   0,
	val pollInterval        : Int         =   0,
	val precision           : Int         =   0,
	val rootDelay           : Double      = 0.0,
	val rootDispersion      : Double      = 0.0,
	val referenceId         : Array[Byte] = Array.ofDim[Byte](4),
	val referenceTimestamp  : Timestamp   = new Timestamp(0.0),
	val originateTimestamp  : Timestamp   = new Timestamp(0.0),
	val receiveTimestamp    : Timestamp   = new Timestamp(0.0),
	val transmitTimestamp   : Timestamp   = Timestamp.now,
	val destinationTimestamp: Timestamp   = new Timestamp(0.0)
)
{
	
	/**
	 * This method constructs the data bytes of a raw NTP packet.
	 */
	def toByteArray : Array[Byte] =
	{
		// All bytes are automatically set to 0
		val p = Array.ofDim[Byte](48)

		p(0) = (leapIndicator << 6 | version << 3 | mode).toByte
		p(1) = stratum.toByte
		p(2) = pollInterval.toByte
		p(3) = precision.toByte
		
		val l = (rootDelay * 65536.0).toInt
		p(4) = ((l >> 24) & 0xFF).toByte
		p(5) = ((l >> 16) & 0xFF).toByte
		p(6) = ((l >> 8) & 0xFF).toByte
		p(7) = (l & 0xFF).toByte
		
		val ul = (rootDispersion * 65536.0).toLong
		p(8)    = ((ul >> 24) & 0xff).toByte
		p(9)    = ((ul >> 16) & 0xff).toByte
		p(10)   = ((ul >> 8) & 0xff).toByte
		p(11)   = (ul & 0xff).toByte
		
		p(12) = referenceId(0)
		p(13) = referenceId(1)
		p(14) = referenceId(2)
		p(15) = referenceId(3)
		
		referenceTimestamp.toBytes(p, 16)
		originateTimestamp.toBytes(p, 24)
		receiveTimestamp.toBytes(p, 32)
		transmitTimestamp.toBytes(p, 40)
		
		p 
	}
	
	
	
	private def referenceIdStr : String =
	    {
		if (stratum==0 || stratum==1)
    		{
			new String(referenceId)
		    }
		else if (version==3)
		    {
			java.net.InetAddress.getByAddress(referenceId).toString
		    }
		else if (version==4)
		    {
			val v = 
			    (toInt(referenceId(0)) /        256.0) + 
				(toInt(referenceId(1)) /      65536.0) +
				(toInt(referenceId(2)) /   16777216.0) +
				(toInt(referenceId(3)) / 4294967296.0)
			 v.toString
		     }
		 else
		     {
		     ""
		     }
    	}

	/**
	 * Returns a string representation of a NtpMessage
	 */
	override def toString() : String =
	    {
		val precisionStr =
			new java.text.DecimalFormat("0.#E0").format(math.pow(2, precision))
			
		val rootFormat = new java.text.DecimalFormat("0.00")
			
		val props = Map(
			
		    "Leap indicator:       " -> leapIndicator,
			"Version:              " -> version,
			"Mode:                 " -> mode,
			"Stratum:              " -> stratum,
			"Poll:                 " -> pollInterval,
			"Precision:            " -> (precision + " (" + precisionStr + " seconds)"),
			"Root delay:           " -> (rootFormat.format(rootDelay*1000) + " ms"),
			"Root dispersion:      " -> (rootFormat.format(rootDispersion*1000) + " ms"),
			"Reference identifier: " -> referenceIdStr,
			"Reference timestamp:  " -> referenceTimestamp.toString,
			"Originate timestamp:  " -> originateTimestamp.toString,
			"Receive timestamp:    " -> receiveTimestamp.toString,
			"Transmit timestamp:   " -> transmitTimestamp.toString
			
			)
			
		props.mkString("\n")
	    }
	
	
	
	private def toInt(b: Byte) : Int =
		b.toInt & 0xff

	
	
	
	
	

    

    def localClockOffset : Timestamp =
        {
		val clockOffset =
			((receiveTimestamp      - originateTimestamp)  +
			 (destinationTimestamp  - transmitTimestamp)) * 0.5
		
		clockOffset
        }

}
	


object NtpMessage
{	
        
    /**
	 * Constructs a new NtpMessage from an array of bytes.
	 */
	def apply(array: Array[Byte]) : NtpMessage =
	{
		// See the packet format diagram in RFC 2030 for details 
		val leapIndicator = (array(0) >> 6) & 0x3
		val version       = (array(0) >> 3) & 0x7
		val mode          = array(0) & 0x7
		val stratum       = toInt(array(1))
		val pollInterval  = array(2)
		val precision     = array(3)
		
		val rootDelay = (array(4) * 256.0) + 
			toInt(array(5)) +
			(toInt(array(6)) / 256.0) +
			(toInt(array(7)) / 65536.0);
		
		val rootDispersion = (toInt(array(8)) * 256.0) + 
			toInt(array(9)) +
			(toInt(array(10)) / 256.0) +
			(toInt(array(11)) / 65536.0);
		
		val referenceIdentifier = Array.ofDim[Byte](4)
		referenceIdentifier(0) = array(12)
		referenceIdentifier(1) = array(13)
		referenceIdentifier(2) = array(14)
		referenceIdentifier(3) = array(15)
		
		val referenceTimestamp   = Timestamp(array, 16)
		val originateTimestamp   = Timestamp(array, 24)
		val receiveTimestamp     = Timestamp(array, 32)
		val transmitTimestamp    = Timestamp(array, 40)
		val destinationTimestamp = Timestamp.now
		
		new NtpMessage(leapIndicator, version, mode, stratum, pollInterval, precision,
		    rootDelay, rootDispersion, referenceIdentifier, referenceTimestamp,
		    originateTimestamp, receiveTimestamp, transmitTimestamp, destinationTimestamp)
	}
	
	private def toInt(b: Byte) : Int =
		b.toInt & 0xff

	private def decodeTimestamp(array: Array[Byte], offset: Int) : Double =
	    {
		var r = 0.0
		for (i <- 0 until 8)
			r += toInt(array(offset+i)) * math.pow(2, (3-i)*8)
		r
	    }
	
	
	
}   


import java.net.{InetAddress, DatagramSocket, DatagramPacket}

class NtpClient(host: String) extends pedro.util.Logged
{
    def ping : Option[NtpMessage] =
        {
        try
            {
            val addr = InetAddress.getByName(host)
            val sock = new DatagramSocket
            sock.setSoTimeout(7000)
            val sBuf = (new NtpMessage).toByteArray
            val sPacket = new DatagramPacket(sBuf, sBuf.length, addr, 123)
            sock.send(sPacket)
            val rBuf = Array.ofDim[Byte](48)
            val rPacket = new DatagramPacket(rBuf, rBuf.length)
            sock.receive(rPacket)
            
            val msg = NtpMessage(rPacket.getData)
            Some(msg)
            }
        catch 
            {
            case e: Exception =>
                error("ping: " + e)
                None
            }
        }
        

}

object NtpClientTest
{
    def test =
        {
        val host1 = "time-b.nist.gov"
        val host2 = "time.uh.edu"
        val cli = new NtpClient(host1)
        val res = cli.ping
        if (res.isEmpty)
            println("Test failed")
        else
            {
            val msg = res.get
            println("Received message! : \n" + msg)
            val localClockOffset = msg.localClockOffset
            println("Local clock offset: " +
			    (new java.text.DecimalFormat("0.00")).format(localClockOffset.v*1000) + " ms")
            }

        }
        
    def testTimestamp =
        {
        val arr1 = Array(0.toByte, 0.toByte, 0.toByte, 1.toByte,
                         128.toByte, 0.toByte, 0.toByte, 0.toByte)
        val t = Timestamp(arr1, 0)
        println("t: " + t.v);
        val arr2 = Array.ofDim[Byte](8)
        t.toBytes(arr2, 0)
        println("arr1:"+ arr1.mkString(","))
        println("arr2:"+ arr2.mkString(","))
        
        }
        
    def main(argv: Array[String]) : Unit =
        {
        test
        }
}




