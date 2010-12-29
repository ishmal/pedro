/**
 * Simple Servlet tools.
 *
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (C) 2010 Bob Jamison
 *
 *  This library is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU Lesser General Public
 *  License as published by the Free Software Foundation; either
 *  version 2.1 of the License, or (at your option) any later version.
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


package pedro.net.servlet


/**
 * Authorization record.  Used to verify that access to RestServlet's
 * handlers is permitted
 */  
trait Auth
{
    val id    : String
    val level : Int
}

case class AuthBasic(id:String, level:Int) extends Auth
case object AuthNone extends Auth
{
    val id = ""
    val level = 0
}


