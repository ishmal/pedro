/**
 *  Test AMQP client capabilities
 *
 *  Author: Bob Jamison
 *  Copyright, 2012
 *    
 *  
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */   

package pedro.net.amqp

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FeatureSpec}
import org.scalatest.matchers.{ShouldMatchers}

@RunWith(classOf[JUnitRunner])
class AmqpTest extends FeatureSpec with ShouldMatchers
{
    import pedro.net.amqp.Encoder._

    feature("The coded can encode and decode information in AMQP format")
        {
        scenario("Stuff is encoded")
            {
            val str = "hello, world"
            val strarr = stringArray("a", "b", "c")("abc")
            val intarr = intArray(1,2,3)(0x0000000100000001L)
            val exp = Array[Byte](
                0x00.toByte, 0xa2.toByte, 0x03.toByte, 0x61.toByte, 0x62.toByte, 0x63.toByte,
                0xe0.toByte, 0x08.toByte, 0x03.toByte, 0xa1.toByte, 
                0x01.toByte, 0x61.toByte, 0x01.toByte, 0x62.toByte, 0x01.toByte, 0x63.toByte)
            expectResult(exp)(strarr.bytes)
            }    
        }

    feature("The AmqpClient must be able to connect with STARTTLS and SASL")
        {
        scenario("Server starts up and client connects")
            {
            val srv = new AmqpServer
            var res = srv.start
            assert(res)
            val cli = new AmqpClient
            res = cli.connect
            assert(res)
            }    
        }

}


