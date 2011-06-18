package pedro.net.amqp

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FeatureSpec,GivenWhenThen}



@RunWith(classOf[JUnitRunner])
class AmqpTest extends FeatureSpec with GivenWhenThen
{
    import pedro.net.amqp.Encoder._

    feature("The coded can encode and decode information in AMQP format")
        {
        scenario("Stuff is encoded")
            {
            given("A String")
            val str = "hello, world"
            when("a string is encoded")
            val strarr = stringArray("a", "b", "c")("abc")
            val intarr = intArray(1,2,3)(0x0000000100000001L)
            then("the output must equal our expected value")
            val exp = Array[Byte](
                0x00.toByte, 0xa2.toByte, 0x03.toByte, 0x61.toByte, 0x62.toByte, 0x63.toByte,
                0xe0.toByte, 0x08.toByte, 0x03.toByte, 0xa1.toByte, 
                0x01.toByte, 0x61.toByte, 0x01.toByte, 0x62.toByte, 0x01.toByte, 0x63.toByte)
            expect(exp)(strarr.bytes)
            }    
        }

    feature("The AmqpClient must be able to connect with STARTTLS and SASL")
        {
        scenario("Server starts up and client connects")
            {
            given("an AMQPServer")
            val srv = new AmqpServer
            when("startup is attempted")
            var res = srv.start
            then("the result should be success")
            assert(res)
            val cli = new AmqpClient
            when("a connection is attempted")
            res = cli.connect
            then("the result should be success")
            assert(res)
            }    
        }

}


