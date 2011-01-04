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



