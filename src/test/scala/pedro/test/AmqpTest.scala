package pedro.test

import pedro.net.amqp._

import org.scalatest.{FeatureSpec,GivenWhenThen}
import org.scalatest.matchers.MustMatchers
 


class AmqpSpec extends FeatureSpec with GivenWhenThen with MustMatchers
{
    feature("The coded can encode and decode information in AMQP format")
        {
        scenario("Stuff is encoded")
            {
            given("A String")
            val str = "hello, world"
            when("a string is encoded")
            var res = Codec.encode(str)
            then("the output must equal our expected value")
            res.isDefined must be === true
            res.get must be === Array[Byte](0xa1.toByte, 0x0c,
                0x68, 0x65, 0x6C, 0x6C, 0x6F, 0x2C, 0x20, 0x77, 0x6F, 0x72, 0x6C, 0x64)

            when("a compound thing is encoded")
            res = Codec.encode(List(1, 2.toByte, "Hello", 4.5))
            then("it's gotta look good")
            res.foreach(println)
            res.isDefined must be === true
            res.get.length must be > 1
            }    
        }

    feature("The AmqpClient must be able to connect with STARTTLS and SASL")
        {
        scenario("The client connects")
            {
            given("an AMQPClient")
            val cli = new AmqpClient("ishmal.org")
            when("a connection is attempted")
            val res = cli.connect
            then("the result should be success")
            res must be === true
            }    
        }

}


