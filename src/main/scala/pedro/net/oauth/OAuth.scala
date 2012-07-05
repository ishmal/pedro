/**
 *  This is a simple implementation of OAuth for Scala.
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


package pedro.net.oauth

import java.io.InputStream

object OAuth
{
    val OAUTH_CONSUMER_KEY          = "oauth_consumer_key"
    val OAUTH_TOKEN                 = "oauth_token"
    val OAUTH_TOKEN_SECRET          = "oauth_token_secret"
    val OAUTH_SIGNATURE_METHOD      = "oauth_signature_method"
    val OAUTH_SIGNATURE             = "oauth_signature"
    val OAUTH_TIMESTAMP             = "oauth_timestamp"
    val OAUTH_NONCE                 = "oauth_nonce"
    val OAUTH_VERSION               = "oauth_version"
    val OAUTH_CALLBACK              = "oauth_callback"
    val OAUTH_CALLBACK_CONFIRMED    = "oauth_callback_confirmed"
    val OAUTH_VERIFIER              = "oauth_verifier"

    val HMAC_SHA1                   = "HMAC-SHA1"
    val RSA_SHA1                    = "RSA-SHA1"
    
    val VERSION_REJECTED            = "version_rejected"
    val PARAMETER_ABSENT            = "parameter_absent"
    val PARAMETER_REJECTED          = "parameter_rejected"
    val TIMESTAMP_REFUSED           = "timestamp_refused"
    val NONCE_USED                  = "nonce_used"
    val SIGNATURE_METHOD_REJECTED   = "signature_method_rejected"
    val SIGNATURE_INVALID           = "signature_invalid"
    val CONSUMER_KEY_UNKNOWN        = "consumer_key_unknown"
    val CONSUMER_KEY_REJECTED       = "consumer_key_rejected"
    val CONSUMER_KEY_REFUSED        = "consumer_key_refused"
    val TOKEN_USED                  = "token_used"
    val TOKEN_EXPIRED               = "token_expired"
    val TOKEN_REVOKED               = "token_revoked"
    val TOKEN_REJECTED              = "token_rejected"
    val ADDITIONAL_AUTHORIZATION_REQUIRED = "additional_authorization_required"
    val PERMISSION_UNKNOWN          = "permission_unknown"
    val PERMISSION_DENIED           = "permission_denied"
    val USER_REFUSED                = "user_refused"

    val OAUTH_ACCEPTABLE_VERSIONS   = "oauth_acceptable_versions"
    val OAUTH_ACCEPTABLE_TIMESTAMPS = "oauth_acceptable_timestamps"
    val OAUTH_PARAMETERS_ABSENT     = "oauth_parameters_absent"
    val OAUTH_PARAMETERS_REJECTED   = "oauth_parameters_rejected"
    val OAUTH_PROBLEM_ADVICE        = "oauth_problem_advice"
    
}


case class OAuthAccessor(name: String)
{
    
}

class OAuthConsumer(val callbackURL: String, val consumerKey: String,
            val consumerSecret: String, val serviceProvider: OAuthServiceProvider)
{
    
}

class OAuthServiceProvider
{
    
}

class OAuthMessage(val method: String, 
            val url: String,
            val params: Map[String, String], 
            val bodyAsStream: Option[InputStream] = None)
{
    
}

object OAuthMessage
{
    def validate(message: OAuthMessage) : Boolean =
        {
        true
        }
}
