/**
 * Minimalist LDAP client
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


import javax.naming.{Context}
import javax.naming.directory.{BasicAttribute, BasicAttributes, DirContext}
import javax.naming.directory.{InitialDirContext, SearchControls, SearchResult}


/**
 * A simple wrapper of the Java runtime's LDAP client abilities.
 *
 * @param host the LDAP server address
 * @param port the port on the LDAP server
 * @param baseDn the distinguished name of the database to query
 * @param orgUnit which user database within the realm defined by the baseDn
 * @param username the user to authenticate or who is performing a search
 * @param password the credential string of the user
 */
class Ldap(host: String, port: Int, baseDn: String,
    orgUnit: String, username: String, password: String) extends pedro.util.Logged
{
    val url = "ldap://" + host + ":" + port + "/" + baseDn
    
    /**
     * Connect to the LDAP server.  Common code called from other methods.
     */
    private def connect : Option[DirContext] =
        {
        val env = new java.util.Hashtable[String, String]
        env.put(Context.INITIAL_CONTEXT_FACTORY, "com.sun.jndi.ldap.LdapCtxFactory")
        env.put(Context.PROVIDER_URL, url)
        env.put(Context.SECURITY_AUTHENTICATION, "simple")
        val principal = "cn=" + username + ", ou=" + orgUnit + ", " + baseDn
        env.put(Context.SECURITY_PRINCIPAL, principal)
        env.put(Context.SECURITY_CREDENTIALS, password)
        try 
            {
            val ctx = new InitialDirContext(env)
            Some(ctx)
            }
        catch
            {
            case e: Exception =>
                error("connect: " + e)
                None
            }
        }
    

    /**
     * Note the redundant parameter of orgUnit.  This is to allow querying
     * of another org than what was used for access credentials.
     */              
    def search(orgUnit: String, query: Map[String, String]) : Map[String, String] =
        {
        val res = connect
        if (!res.isDefined)
            return Map()

        val map = scala.collection.mutable.Map[String, String]()

        val ctx = res.get
                
        val matchAttrs = new BasicAttributes(true)
        for (e <- query)
            matchAttrs.put(new BasicAttribute(e._1, e._2))
    
        try 
            {
            val res = ctx.search("ou="+orgUnit, matchAttrs)
            while (res.hasMore)
                {
                val result = res.next.asInstanceOf[SearchResult]
                val attribs = result.getAttributes
                val values = attribs.getAll
                while (values.hasMore) 
                    {
                    val attr = values.next.asInstanceOf[BasicAttribute]
                    map += attr.getID -> attr.get.toString
                    }
                }

            } 
        catch
            {
            case e: Exception =>
                error("search: " + e)
            }
        finally
            {
            ctx.close
            }
        map.toMap
        }


    def authenticate : Boolean =
        {
        val res = connect
        res.foreach(_.close)
        res.isDefined
        }

}


object Ldap
{
    val host     =  "ldap.example.com"
    val port     =  389 
    val baseDn   = "dc=example,dc=com"
    val orgUnit  = "User"
    val username = "joeuser"
    val password = "joepass"
    
    def test1 =
        {
        val ldap = new Ldap(host, port, baseDn, orgUnit, username, password)
        val res = ldap.authenticate
        println("auth:" + res)
        }

    def test2 =
        {
        val ldap = new Ldap(host, port, baseDn, orgUnit, username, password)
        val res = ldap.search(orgUnit, Map("sn" -> "Castillo"))
        println("search:" + res)
        }


    def main(argv: Array[String]) =
        {
        test1
        test2
        }

}


