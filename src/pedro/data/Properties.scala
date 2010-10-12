/**
 * Scala wrapper of Java properties file.
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (C) 2010 Bob Jamison
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

package pedro.data



/**
 * A very simple Scala wrapper of Properties.  Loads and/or stores a
 * map of name->value.  Note that there is a default value of "", so
 * using a nonexistant name will not cause an exception. 
 */ 
object Properties
{
    //bug workaround: copied from immutable.Map.scala
    //remove when fixed
    class WithDefault[A, +B](underlying: Map[A, B], d: A => B) extends Map[A, B] {
        override def size = underlying.size
        def get(key: A) = underlying.get(key) orElse Some(default(key))
        def iterator = underlying.iterator
        override def empty = new WithDefault(underlying.empty, d)
        override def updated[B1 >: B](key: A, value: B1): WithDefault[A, B1] = new WithDefault[A, B1](underlying.updated[B1](key, value), d)
        override def + [B1 >: B](kv: (A, B1)): WithDefault[A, B1] = updated(kv._1, kv._2)
        def - (key: A): WithDefault[A, B] = new WithDefault(underlying - key, d)
        override def default(key: A): B = d(key)
    }


    def loadFile(fname: String) : Option[Map[String,String]] =
        {
        try
            {
            val file = new java.io.FileInputStream(fname)
            val props = new java.util.Properties
            props.load(file)
            file.close
            val iter = props.entrySet.iterator
            val vals = scala.collection.mutable.Map[String,String]()
            while (iter.hasNext) //dont really need JavaConversion
                {
                val item = iter.next
                vals += item.getKey.toString -> item.getValue.toString
                }
            val res = new WithDefault[String,String](vals.toMap, x => "")
            Some(res)
            }
        catch
            {
            case e:Exception => pedro.log.error("Properties.loadFile: " + e)
            None
            }
        }    

    def saveFile(sprops: Map[String,String], fname: String) : Boolean =
        {
        try
            {
            val jprops = new java.util.Properties
            sprops.foreach(a=> jprops.put(a._1, a._2))
            val file = new java.io.FileOutputStream(fname)
            jprops.store(file, "Scala Properties: " + fname)
            file.close
            true
            }
        catch
            {
            case e:Exception => pedro.log.error("Properties.saveFile: " + e)
            false
            }
        }    

    /*
    def main(argv: Array[String]) =
        {
        val props = Properties.loadFile("props.ini")
        if (props.isEmpty)
            pedro.log.error("Properties test failed")
        else
            {
            val p = props.get
            println(p("a.b.c.d.e"))
            Properties.saveFile(p, "props2.ini")
            }
        }
    */
}



