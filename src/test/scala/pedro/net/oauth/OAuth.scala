/**
 *  This is a simple implementation of OAuth for Scala.
 *
 *  Author: Bob Jamison
 *  Copyright, 2011
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

package pedro.net.oauth

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.{FeatureSpec,GivenWhenThen}



@RunWith(classOf[JUnitRunner])
class OAuth1Test extends FeatureSpec with GivenWhenThen
{
    val scalaNode = OAuth1Account(
        name              = "ScalaNode", 
        consumerKey       = "HANSFDpLzvHUTthlX1hWyQ",
        consumerSecret    = "Gv4N0W4wctWZgYw12XawReBRZuDjTbDOFYVAfme0",
        requestTokenURL   = "https://api.twitter.com/oauth/request_token",
        authURL           = "https://api.twitter.com/oauth/authorize",
        accessURL         = "https://api.twitter.com/oauth/access_token",
        accessToken       = "148793247-itlErNBMqmRLeA2UAjWxSglFFPM7wifWfy48zMw2",
        accessTokenSecret = "FMvcp4zuYBnKPIKj0Wj7Qq7qb1ldxTD9rDi1Q4uYQo"
        )

    feature("OAuth v1 client can connect with and use an authenticated service")
        {
        scenario("Check twitter updates")
            {
            val oauth = new OAuth1Client(scalaNode, true)
            val res = oauth.doPost("http://api.twitter.com/1/statuses/update.json",
                Map("status"->"fixed again"))
            assert(res)
            }    

        scenario("Perform a twitter search")
            {
            val oauth = new OAuth1Client(scalaNode, true)
            val res = oauth.doGet("http://search.twitter.com/search.json", Map("q"->"#earthquake"))
            assert(res.isDefined)
            if (res.isDefined)
                println("##### result:\n" + res.get)
            }    
        }

}


@RunWith(classOf[JUnitRunner])
class OAuth2Test extends FeatureSpec with GivenWhenThen
{
    val scalaNode = OAuth2Account(
        name           = "ScalaNode", 
        clientId       = "126170977420983",
        clientSecret   = "9920407ac77e7688591d32aed44aa155",
        authURL        = "https://graph.facebook.com/oauth/authorize",
        accessTokenURL = "https://graph.facebook.com/oauth/access_token",
        redirectURL    = "https://www.facebook.com/connect/login_success.html",
        accessToken    = "126170977420983|c_IyFUdR73xZR-sPKd1SXrnyYds."
        )
    

    feature("OAuth v2 client can connect with and use an authenticated service")
        {
        scenario("Check twitter updates")
            {
            val oauth = new OAuth2Client(scalaNode)

            val res = oauth.doGet("https://graph.facebook.com/btaylor")
            assert(res.isDefined)
            if (res.isDefined)
                println("##### result:\n" + res.get)
            }    
        }


}


