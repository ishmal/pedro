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


/**
 * Some handy constants
 */ 
object Amqp
{
    val normalProto = "AMQP\u00d0\u0001\u0000\u0000".getBytes
    val tlsProto    = "AMQP\u00d2\u0001\u0000\u0000".getBytes
    val saslProto   = "AMQP\u00d3\u0001\u0000\u0000".getBytes
}

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


class AmqpValue
{
    //override for each type
    def typ   : Int = 0
    //override for each type
    def size  : Int = 0   //number of bytes after constructor or count
    //override for each type
    def widen : AmqpValue = this
    //override for each type
    def body  : Array[Byte] = Array()


    val desc  : Option[AmqpDescriptor]=None

    def bytes : Array[Byte] = 
        if (desc.isDefined)
            desc.get.bytes ++ Array[Byte](typ.toByte) ++ body
        else
            Array[Byte](typ.toByte) ++ body
    
    /**
     * Every AmqpValue type will have an apply(key) and apply(index) method,
     * so that we may chain apply()'s together as a path, like:
     *    aval("hello")("world")(3)
     * ...should return a AmqpInt(6)               
     * We will provide no-val returns for inappropriate types, so the user
     * need not worry about exceptions.     
     */              
    
    /**
     * Return a named property of a AmqpObject.  Classes other than AmqpMap
     *      return AmqpNil
     * Usage:   val v = aval("fieldName")    
     */
    def apply(name: String) : AmqpValue =
        new AmqpNull

    /**
     * Return an indexed member of a AmqpArray.  Classes other than AmqpArray
     * will return AmqpNull.  An index out of bounds will also return AmqpNull.        
     */
    def apply(indx: Int) : AmqpValue =
        new AmqpNull

    /**
     * Return the internal Map implementation of a AmqpObject.  Classes other
     * than AmqpMap will return an empty map.  This is useful for walking an
     * object tree
     */              
    def toMap : Map[String, AmqpValue] = Map()

    /**
     * Convenience method.  Iterate over the children of a AmqpArray, applying
     * f() to each one.  Classes other than AmqpArray will do nothing.
     */              
    def foreach(f: (AmqpValue) => Unit): Unit = {}

    /**
     * Convenience method.  Map each object of a AmqpArray to type U, returning
     * a List of type U.  Classes other than AmqpArray will return an empty
     * List.     
     */              
    def map[U](f: (AmqpValue) =>  U): Seq[U] = List()

    /**
     * Return a List of the members of a AmqpArray, to allow the use of List's API
     * on the members of the List.  Classes other than AmqpArray will return an empty
     * List.     
     */              
    def toList : Seq[AmqpValue] = List()

    /**
     * Return the double value of a AmqpDouble, else try to convert.
     * Notice that we will accept the member of a single-member array.     
     */              
    def d : Double =
        this match
            {
            case v:AmqpTrue     => 1.0
            case v:AmqpFalse    => 0.0
            case v:AmqpInt      => v.value.toDouble
            case v:AmqpDouble   => v.value
            case v:AmqpArray[_] => if (v.value.size==1) v.value(0).d else 0.0
            case v:AmqpString   => toDouble(v.value)
            case _              => 0.0
            }

    /**
     * Return the long value of a AmqpInt, else try to convert
     */              
    def i : Long =
        this match
            {
            case v:AmqpTrue     => 1
            case v:AmqpFalse    => 0
            case v:AmqpInt      => v.value
            case v:AmqpDouble   => v.value.toLong
            case v:AmqpArray[_] => if (v.value.size==1) v.value(0).i else 0
            case v:AmqpString   => toLong(v.value)
            case _              => 0
            }

    /**
     * Return the string value of a AmqpString, else try to convert
     */              
    def s : String =
        this match
            {
            case v:AmqpString   => v.value
            case v:AmqpArray[_] => if (v.value.size==1) v.value(0).s else ""
            case v:AmqpInt      => v.value.toString
            case v:AmqpDouble   => v.value.toString
            case v:AmqpTrue     => "true"
            case v:AmqpFalse    => "false"
            case _              => ""
            }

    /**
     * Return the boolean value of a AmqpBoolean, else try to convert
     */              
    def b : Boolean =
        this match
            {
            case v:AmqpTrue     => true
            case v:AmqpFalse    => false
            case v:AmqpInt      => (v.value != 0)
            case v:AmqpDouble   => (v.value != 0.0)
            case v:AmqpArray[_] => if (v.value.size==1) v.value(0).b else false
            case v:AmqpString   => v.value.trim.equalsIgnoreCase("true")
            case _              => false
            }


    //============================
    //= UTILITY METHODS
    //============================
       

    protected def toDouble(s: String) : Double =
        try { s.trim.toDouble }
        catch { case e:Exception => 0.0 }
    protected def toLong(s: String) : Long =
        try { s.trim.toLong }
        catch { case e:Exception => 0 }
    protected def toBytes(v: Short) =
        Array[Byte]( ((v>>8)&0xff).toByte, (v&0xff).toByte)
    protected def toBytes(v: Int) =
        Array[Byte]( ((v>>24)&0xff).toByte, ((v>>16)&0xff).toByte,
                     ((v>> 8)&0xff).toByte, ((v    )&0xff).toByte)
    protected def toBytes(v: Long) =
        Array[Byte]( ((v>>56)&0xff).toByte, ((v>>48)&0xff).toByte,
                     ((v>>40)&0xff).toByte, ((v>>32)&0xff).toByte,
                     ((v>>24)&0xff).toByte, ((v>>16)&0xff).toByte,
                     ((v>> 8)&0xff).toByte, ((v    )&0xff).toByte)

}

/**
 * This is the optional descriptor that precedes a type constructor.
 * The value it wraps can be any AmqpValue, but it -should- be an
 * AmqpSymbol or AmqpULong  
 *
 * It is serialized as:
 *     0x00 <child_constructor> <child_body>  
 */ 
case class AmqpDescriptor(value: AmqpValue) extends AmqpValue
{
    override def typ = 0x00
    override def body = value.bytes
}


//########################################################################
//# FIXED, 0
//########################################################################

case class AmqpNull(
    override val desc: Option[AmqpDescriptor]=None
) extends AmqpValue
{
    override def typ = 0x40
}
case class AmqpTrue(
    override val desc: Option[AmqpDescriptor]=None
) extends AmqpValue
{
    override def typ = 0x41
}
case class AmqpFalse(
    override val desc: Option[AmqpDescriptor]=None
) extends AmqpValue
{
    override def typ = 0x42
}


//########################################################################
//# FIXED, 1
//########################################################################
case class AmqpUByte(
    val value : Byte = 0x00.toByte,
    override val desc: Option[AmqpDescriptor]=None
) extends AmqpValue
{
    override def typ   = 0x50
    override def size  = 1
    override def body = Array[Byte](value)
}
case class AmqpByte(
    val value : Byte = 0x00.toByte,
    override val desc: Option[AmqpDescriptor]=None
) extends AmqpValue
{
    override def typ   = 0x51
    override def size  = 1
    override def body = Array[Byte](value)
}

//########################################################################
//# FIXED, 2
//########################################################################
case class AmqpUShort(
    val value: Short = 0,
    override val desc: Option[AmqpDescriptor]=None
) extends AmqpValue
{
    override def typ   = 0x60
    override def size  = 2
    override def body = toBytes(value)
}
case class AmqpShort(
    val value: Short = 0,
    override val desc: Option[AmqpDescriptor]=None
) extends AmqpValue
{
    override def typ   = 0x61
    override def size  = 2
    override def body = toBytes(value)
}

//########################################################################
//# FIXED, 4
//########################################################################

case class AmqpUInt(
    val value: Int = 0,
    override val desc: Option[AmqpDescriptor]=None
) extends AmqpValue
{
    override def typ   = 0x70
    override def size  = 4
    override def body = toBytes(value)
}
case class AmqpInt(
    val value: Int = 0,
    override val desc: Option[AmqpDescriptor]=None
) extends AmqpValue
{
    override def typ   = 0x71
    override def size  = 4
    override def body = toBytes(value)
}
case class AmqpFloat(
    val value: Float = 0.0f,
    override val desc: Option[AmqpDescriptor]=None
) extends AmqpValue
{
    override def typ   = 0x72
    override def size  = 4
    override def body = toBytes(java.lang.Float.floatToIntBits(value))
}


//########################################################################
//# FIXED, 8
//########################################################################

case class AmqpULong(
    val value: Long = 0L,
    override val desc: Option[AmqpDescriptor]=None
) extends AmqpValue
{
    override def typ   = 0x80
    override def size  = 8
    override def body = toBytes(value)
}
case class AmqpLong(
    val value: Long = 0L,
    override val desc: Option[AmqpDescriptor]=None
) extends AmqpValue
{
    override def typ   = 0x81
    override def size  = 8
    override def body = toBytes(value)
}
case class AmqpDouble(
    val value: Double = 0.0,
    override val desc: Option[AmqpDescriptor]=None
) extends AmqpValue
{
    override def typ   = 0x82
    override def size  = 8
    override def body = toBytes(java.lang.Double.doubleToLongBits(value))
}
case class AmqpTimestamp(
    val value: Long = 0L,
    override val desc: Option[AmqpDescriptor]=None
) extends AmqpValue
{
    override def typ   = 0x83
    override def size  = 8
    override def body = toBytes(value)
}


//########################################################################
//# UUID (16 bytes)
//########################################################################
case class AmqpUuid(
    val hi: Long = 0L,
    val lo: Long = 0L,
    override val desc: Option[AmqpDescriptor]=None
) extends AmqpValue
{
    override def typ   = 0x98
    override def size  = 16
    override def body = toBytes(hi) ++ toBytes(lo)
}


//########################################################################
//# Variable 1 and 4
//########################################################################

case class AmqpBinary(
    val value: Array[Byte] = Array(),
    val wide: Boolean = false,
    override val desc: Option[AmqpDescriptor]=None
) extends AmqpValue
{
    override def typ   = if (!wide && value.size < 256) 0xa0 else 0xb0
    override def size  = bytes.size
    override def body =
        if (wide || value.size < 256)
            Array[Byte](value.length.toByte) ++ value
        else
            toBytes(value.length.toByte) ++ value
    override def widen = copy(wide=true)
}

case class AmqpString(
    val value: String = "",
    val wide: Boolean = false,
    override val desc: Option[AmqpDescriptor]=None
) extends AmqpValue
{
    override def typ   = if (!wide && value.size < 256) 0xa1 else 0xb1
    override def size  = bytes.size
    override def body =
        if (wide || value.size < 256)
            Array[Byte](value.length.toByte) ++ value.getBytes
        else
            toBytes(value.length.toByte) ++ value.getBytes
    override def widen = copy(wide=true)
}

case class AmqpSymbol(
    val value: String = "",
    val wide: Boolean = false,
    override val desc: Option[AmqpDescriptor]=None
) extends AmqpValue
{
    override def typ   = if (!wide && value.size < 256) 0xa2 else 0xb2
    override def size  = bytes.size
    override def body =
        if (wide || value.size < 256)
            Array[Byte](value.length.toByte) ++ value.getBytes
        else
            toBytes(value.length.toByte) ++ value.getBytes
    override def widen = copy(wide=true)
}



/**
 * This is the only parameterized class in the AMQP data model.  This is to
 * make it clear to the coder that the array's children must all be the same
 * subclass of AmqpValue.   Not only that, but they must be the same size.
 * This restriction make the child_constructor required only once, with each
 * child only requiring the body after the constructor.  
 *
 * This class is serialized as:
 *     {descriptor} 0xe0 <size8> <count8> <child_constructor> <count*children>
 * or if total bytes or the count cannot fit into 8 bits:
 *     {descriptor} 0xf0 <size32> <count32> <child_constructor><count*children>
 * For example, Encoder.array(1,2,3) would be serialized as:
 *     0xe0 0x0e 0x03 0x71 0x00000001 0x00000002 0x00000003  
 * 
 * Note that variable-sized items such as this array, List, String, Binary,
 * Symbol, and Map can be encoded with either 8-bit dimensions, or "wide"
 * 32-bit dimensions.   Since all must be alike, then a builder for this must
 * make sure that all sizes are either all small or all large.  If the check
 * of the children fails the "all small" test, then each of the variable
 * classes has a widen() method to aid in making them all large.              
 */
case class AmqpArray[T<:AmqpValue](
    val value: Seq[T] = Seq(),
    val wide: Boolean = false,
    override val desc: Option[AmqpDescriptor]=None
) extends AmqpValue
{
    override def typ =
        if (!wide && childBytes.size < 256 && value.size < 256) 0xe0 else 0xf0

    override def size = 
        if (isSmall)
            3 + childBytes.size
        else
            9 + childBytes.size

    override def body =
        if (isSmall)
            Array[Byte](size.toByte, value.size.toByte, value(0).typ.toByte) ++ childBytes
        else
            toBytes(size) ++ toBytes(value.size) ++ Array[Byte](value(0).typ.toByte) ++ childBytes

    lazy val childBytes = value.map(_.body).flatten.toArray
    
    def isSmall = !wide && childBytes.size < 256 && value.size < 256
    
    override def widen = copy(wide=true)

    override def apply(index: Int) =
       try
            { value(index) }
        catch
            { case e: IndexOutOfBoundsException => new AmqpNull }

    override def foreach(f: (AmqpValue) => Unit): Unit =
        value.foreach(f)

    override def map[U](f: (AmqpValue) =>  U): Seq[U] =
        value.map(f)

    override def toList : Seq[AmqpValue] =
        value

}


//########################################################################
//# COMPOUND, 1&4
//########################################################################


/**
 * This class contains a sequence of AmqpValues, with no restriction on their
 * types.
 * 
 *  The class is serialized as:
 * 
 *     {descriptor} 0xc0 <size8> <count8> < count * <child_constructor><child_body> >   
 * or if the total byte size or count cannot fit into 8 bits: 
 *     {descriptor} 0xc0 <size32> <count32> < count * <child_constructor><child_body> >   
 *
 * This is much simpler than AmqpArray! 
 */
case class AmqpList(
    val value: Seq[AmqpValue] = Seq(),
    val wide: Boolean = false,
    override val desc: Option[AmqpDescriptor] = None
) extends AmqpValue
{
    override def typ = if (isSmall) 0xc0 else 0xd0

    override def size = bytes.size
    
    override def body =
        if (isSmall)
            Array[Byte](childBytes.size.toByte, value.size.toByte) ++ childBytes
        else
            toBytes(childBytes.size) ++ toBytes(value.size) ++ childBytes

    val isSmall = !wide && childBytes.size < 256 && value.size < 256

    override def widen = copy(wide=true)
    
    lazy val childBytes = value.map(v=> v.bytes).flatten.toArray
    
    override def apply(indx: Int) =
       try
            { value(indx) }
        catch
            { case e: IndexOutOfBoundsException => new AmqpNull }

    override def foreach(f: (AmqpValue) => Unit): Unit =
        value.foreach(f)

    override def map[U](f: (AmqpValue) =>  U): Seq[U] =
        value.map(f)

    override def toList : Seq[AmqpValue] =
        value
}



//########################################################################
//# MAP
//########################################################################


/**
 * This class maintains a map of Key->Value, where the key is an AmqpSymbol, and
 * the value is any AmqpValue. 
 * 
 *  The class is serialized as:
 * 
 *     {descriptor} 0xc1 <size8> <count8> < count/2 * <symbol_constructor><symbol_body<value_constructor><value_body> >   
 * or if the total byte size or count cannot fit into 8 bits: 
 *     {descriptor} 0xd1 <size32> <count32> < count/2 * <symbol_constructor><symbol_body<value_constructor><value_body> >   
 *
 * Note that the count is the total number of elements in the Map, each pair counting
 * as two.  So a "count8" map must have no more than 127 pairs.   
 */
case class AmqpMap(
    val value: Map[AmqpSymbol, AmqpValue] = Map(),
    val wide: Boolean = false,
    override val desc: Option[AmqpDescriptor] = None
) extends AmqpValue
{
    override def typ = if (isSmall) 0xc1 else 0xd1

    override def size = bytes.size
    
    override def body =
        if (isSmall)
            Array[Byte](childBytes.size.toByte, value.size.toByte) ++ childBytes
        else
            toBytes(childBytes.size) ++ toBytes(value.size) ++ childBytes

    val isSmall = !wide && childBytes.size < 256 && value.size < 128 //note, each pair counts as two here

    override def widen = copy(wide=true)
    
    lazy val childBytes = value.toSeq.map(v=> v._1.bytes ++ v._2.bytes).flatten.toArray
    
    
    override def apply(key: String) =
        value.getOrElse(AmqpSymbol(key), new AmqpNull)
}



//########################################################################
//# ENCODER
//########################################################################


object Encoder
{
    implicit def string2descriptor(name: String) : Option[AmqpDescriptor] =
        if (name==null || name.length==0) None else Some(new AmqpDescriptor(new AmqpSymbol(name)))

    implicit def long2descriptor(code: Long) : Option[AmqpDescriptor] =
        Some(new AmqpDescriptor(new AmqpULong(code)))
            
    def desc(name: String) =
        new AmqpDescriptor(new AmqpSymbol(name))
            
    def desc(code: Long) =
        new AmqpDescriptor(new AmqpULong(code))
            
    def array[T<:AmqpValue](values: T*)(implicit desc: Option[AmqpDescriptor]=None) =
        {
        val arr = if (values.forall(_.size < 256)) values else values.map(_.widen)
        new AmqpArray(arr.toSeq, false, desc)
        }
    
    def stringArray(values: String*)(implicit desc: Option[AmqpDescriptor]=None) : AmqpArray[AmqpString] =
        {
        val raw = values.map(s=> new AmqpString(s))
        val arr = if (raw.forall(_.size < 256)) raw else raw.map(_.widen)
        new AmqpArray(arr.toSeq, false, desc)
        }
    
    def intArray(values: Int*)(implicit desc: Option[AmqpDescriptor]=None) : AmqpArray[AmqpInt] =
        new AmqpArray(values.map(i=> new AmqpInt(i)).toSeq, false, desc)
    
    def list(values: AmqpValue*) = AmqpList(values)
    
    def str(s: String) = AmqpString(s)

    def sym(s: String) = AmqpSymbol(s)

    def nul = new AmqpNull
    
}




//########################################################################
//# DECODER
//########################################################################


class Decoder(parsebuf: Array[Byte]) extends pedro.util.Logged
{   
    val len = parsebuf.size
    
    def get(p: Int) : Int =
        {
        try
            { parsebuf(p).toInt }
        catch
            { case e: ArrayIndexOutOfBoundsException => -1 }
        }
    
    def getShort(p: Int) : (Int, Short) =
        {
        if (p+2>=len)
            (-1, 0)
        else
            {
            (p+2, (((parsebuf(p)&0xff) << 8) | ((parsebuf(p+1)&0xff))).toShort)
            }
        }
    
    def getInt(p: Int) : (Int, Int) =
        {
        if (p+4>=len)
            (-1, 0)
        else
            {
            (p+4, 
                (((parsebuf(p  )&0xff) <<24) | ((parsebuf(p+1)&0xff)<<16) |
                 ((parsebuf(p+2)&0xff) << 8) | ((parsebuf(p+3)&0xff)    ))
            )
            }
        }
    
    def getLong(p: Int) : (Int, Long) =
        {
        if (p+8>=len)
            (-1, 0)
        else
            {
            (p+8, 
                (((parsebuf(p  )&0xff) <<56) | ((parsebuf(p+1)&0xff)<<48) |
                 ((parsebuf(p+2)&0xff) <<40) | ((parsebuf(p+3)&0xff)<<32) |
                 ((parsebuf(p+4)&0xff) <<24) | ((parsebuf(p+5)&0xff)<<16) |
                 ((parsebuf(p+6)&0xff) << 8) | ((parsebuf(p+7)&0xff)    ))
            )
            }
        }
    

    def parse(p0: Int, desc: Option[AmqpDescriptor] = None, arrayCode: Int = 0) : (Int,AmqpValue) =
        {
        var p = p0
        val ch = 
            if (arrayCode > 0)
                arrayCode
            else 
                { val c = get(p) ; p += 1 ; c }
        if (ch < 0)
            (-1, new AmqpNull)
        else
            {
            ch match
                {
                case 0x00 => //descriptor
                    val v = parse(p+1)
                    if (v._1 < 0)
                        (-1, new AmqpNull)
                    else
                        parse(v._1, Some(new AmqpDescriptor(v._2)))
                case 0x40 =>
                    (p, new AmqpNull(desc))
                case 0x41 =>
                    (p, new AmqpTrue(desc))
                case 0x42 =>
                    (p, new AmqpFalse(desc))
                case 0x50 =>
                    (p+1, new AmqpUByte(get(p).toByte, desc))
                case 0x51 =>
                    (p+1, new AmqpByte(get(p).toByte, desc))
                case 0x60 => 
                    val r = getShort(p)
                    if (r._1 < 0)
                        (-1, new AmqpNull)
                    else
                        (r._1, new AmqpUShort(r._2, desc))
                case 0x61 =>
                    val r = getShort(p)
                    if (r._1 < 0)
                        (-1, new AmqpNull)
                    else
                        (r._1, new AmqpShort(r._2, desc))
                case 0x70 => 
                    val r = getInt(p)
                    if (r._1 < 0)
                        (-1, new AmqpNull)
                    else
                        (r._1, new AmqpUInt(r._2, desc))
                case 0x71 =>
                    val r = getInt(p)
                    if (r._1 < 0)
                        (-1, new AmqpNull)
                    else
                        (r._1, new AmqpInt(r._2, desc))
                case 0x72 =>
                    val r = getInt(p)
                    if (r._1 < 0)
                        (-1, new AmqpNull)
                    else
                        (r._1, new AmqpFloat(java.lang.Float.intBitsToFloat(r._2), desc))
                case 0x80 => 
                    val r = getLong(p)
                    if (r._1 < 0)
                        (-1, new AmqpNull)
                    else
                        (r._1, new AmqpULong(r._2, desc))
                case 0x81 =>
                    val r = getLong(p)
                    if (r._1 < 0)
                        (-1, new AmqpNull)
                    else
                        (r._1, new AmqpLong(r._2, desc))
                case 0x82 =>
                    val r = getLong(p)
                    if (r._1 < 0)
                        (-1, new AmqpNull)
                    else
                        (r._1, new AmqpDouble(java.lang.Double.longBitsToDouble(r._2), desc))
                case 0x83 =>
                    val r = getLong(p)
                    if (r._1 < 0)
                        (-1, new AmqpNull)
                    else
                        (r._1, new AmqpTimestamp(r._2, desc))
                case 0x98 =>
                    val hi = getLong(p)
                    val lo = getLong(hi._1)
                    if (lo._1 < 0)
                        (-1, new AmqpNull)
                    else
                        (lo._1, new AmqpUuid(hi._2, lo._2, desc))
                case 0xa0 =>
                    val size = get(p)
                    if (size < 0)
                        (-1, new AmqpNull)
                    else
                        {
                        val v = Array.tabulate[Byte](size)(i=>get(p+i).toByte)
                        (p + size, new AmqpBinary(v))
                        }
                case 0xa1 =>
                    val size = get(p)
                    if (size < 0)
                        (-1, new AmqpNull)
                    else
                        {
                        p += 1
                        val v = Array.tabulate[Byte](size)(i=>get(p+i).toByte)
                        (p + size, new AmqpString(new String(v)))
                        }
                case 0xa2 =>
                    val size = get(p)
                    if (size < 0)
                        (-1, new AmqpNull)
                    else
                        {
                        p += 1
                        val v = Array.tabulate[Byte](size)(i=>get(p+i).toByte)
                        (p + size, new AmqpString(new String(v)))
                        }
                case 0xa3 =>
                    val size = get(p)
                    if (size < 0)
                        (-1, new AmqpNull)
                    else
                        {
                        p += 1
                        val v = Array.tabulate[Byte](size)(i=>get(p+i).toByte)
                        (p + size, new AmqpSymbol(new String(v)))
                        }
                case 0xb0 =>
                    val size = getInt(p)
                    if (size._1 < 0)
                        (-1, new AmqpNull)
                    else
                        {
                        p = size._1
                        val v = Array.tabulate[Byte](p)(i=>get(p+i).toByte)
                        (p + size._2, new AmqpBinary(v))
                        }
                case 0xb1 =>
                    val size = getInt(p)
                    if (size._1 < 0)
                        (-1, new AmqpNull)
                    else
                        {
                        p = size._1
                        val v = Array.tabulate[Byte](p)(i=>get(p+i).toByte)
                        (p + size._2, new AmqpString(new String(v)))
                        }
                case 0xb2 =>
                    val size = getInt(p)
                    if (size._1 < 0)
                        (-1, new AmqpNull)
                    else
                        {
                        p = size._1
                        val v = Array.tabulate[Byte](p)(i=>get(p+i).toByte)
                        (p + size._2, new AmqpString(new String(v)))
                        }
                case 0xb3 =>
                    val size = getInt(p)
                    if (size._1 < 0)
                        (-1, new AmqpNull)
                    else
                        {
                        p = size._1
                        val v = Array.tabulate[Byte](p)(i=>get(p+i).toByte)
                        (p + size._2, new AmqpSymbol(new String(v)))
                        }
                case 0xc0 =>
                    val size  = get(p) ; p += 1
                    val count = get(p) ; p += 1
                    if (count < 0)
                        (-1, new AmqpNull)
                    else
                        {
                        val buf = scala.collection.mutable.ListBuffer[AmqpValue]()
                        for (i <- 0 until count)
                            {
                            val r = parse(p, None)
                            if (r._1 < 0)
                                return (-1, new AmqpNull)
                            p = r._1
                            buf += r._2
                            }
                        (p, new AmqpArray(buf.toSeq, false, desc))
                        }
                case 0xc1 =>
                    val size  = get(p) ; p += 1
                    val count = get(p) ; p += 1
                    if (count < 0)
                        (-1, new AmqpNull)
                    else
                        {
                        val buf = scala.collection.mutable.ListBuffer[(AmqpSymbol, AmqpValue)]()
                        for (i <- 0 until count/2)
                            {
                            val key   = parse(p, None)
                            val value = parse(key._1, None)
                            if (value._1 < 0)
                                return (-1, new AmqpNull)
                            p = value._1
                            key._2 match
                                {
                                case k:AmqpSymbol => buf += ((k, value._2))
                                case _ => error("expected symbol for map key")
                                    return (-1, new AmqpNull)
                                }
                            
                            }
                        (p, new AmqpMap(buf.toMap, false, desc))
                        }
                case 0xd0 =>
                    val size  = getInt(p)
                    val count = getInt(size._1)
                    if (count._1 < 0)
                        (-1, new AmqpNull)
                    else
                        {
                        p = count._1
                        val buf = scala.collection.mutable.ListBuffer[AmqpValue]()
                        for (i <- 0 until count._2)
                            {
                            val r = parse(p, None)
                            if (r._1 < 0)
                                return (-1, new AmqpNull)
                            p = r._1
                            buf += r._2
                            }
                        (p, new AmqpArray(buf.toSeq, false, desc))
                        }
                case 0xd1 =>
                    val size  = getInt(p)
                    val count = getInt(size._1)
                    if (count._1 < 0)
                        (-1, new AmqpNull)
                    else
                        {
                        p = count._1
                        val buf = scala.collection.mutable.ListBuffer[(AmqpSymbol, AmqpValue)]()
                        for (i <- 0 until count._2/2)
                            {
                            val key   = parse(p, None)
                            val value = parse(key._1, None)
                            if (value._1 < 0)
                                return (-1, new AmqpNull)
                            p = value._1
                            key._2 match
                                {
                                case k:AmqpSymbol => buf += ((k, value._2))
                                case _ => error("expected symbol for map key")
                                    return (-1, new AmqpNull)
                                }
                            
                            }
                        (p, new AmqpMap(buf.toMap, false, desc))
                        }
                case 0xe0 =>
                    val size  = get(p) ; p += 1
                    val count = get(p) ; p += 1
                    val typ   = get(p) ; p += 1
                    if (size < 0 || count < 0 || typ< 0)
                        (-1, new AmqpNull)
                    else
                        {
                        val buf = scala.collection.mutable.ListBuffer[AmqpValue]()
                        for (i <- 0 until count)
                            {
                            val r = parse(p, None, typ)
                            if (r._1 < 0)
                                return (-1, new AmqpNull)
                            p = r._1
                            buf += r._2
                            }
                        (p, new AmqpArray(buf.toSeq, false, desc))
                        }
                case 0xf0 =>
                    val size  = getInt(p) ; p = size._1
                    val count = getInt(p) ; p = count._1
                    val typ   = get(p) ; p += 1
                    if (size._1 < 0 || count._1 < 0 || typ< 0)
                        (-1, new AmqpNull)
                    else
                        {
                        val buf = scala.collection.mutable.ListBuffer[AmqpValue]()
                        for (i <- 0 until count._2)
                            {
                            val r = parse(p, None, typ)
                            if (r._1 < 0)
                                return (-1, new AmqpNull)
                            p = r._1
                            buf += r._2
                            }
                        (p, new AmqpArray(buf.toSeq, false, desc))
                        }
                case _ =>
                    error("parse: case not handled :" + ch)
                    (-1, new AmqpNull)
                }
            }
        }

    def parse(buf: Array[Byte]) : Option[AmqpValue] =
        {
        val res = parse(0)
        if (res._1 < 0)
            None
        else
            Some(res._2)
        }
}



object Codec extends pedro.util.Logged
{

}

