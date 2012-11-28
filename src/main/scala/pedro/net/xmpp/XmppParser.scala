/**
 * Pedro XMPP Parser
 *
 * Authors:
 *   Bob Jamison
 *
 * Copyright (C) 2010-2012 Bob Jamison
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


package pedro.net.xmpp


import pedro.data.{Attribute, Element}



/**
 *
 */ 
class XmppParser
{

   //Things to save on the stack for each element
    class StackItem(val name: String, val attrs: Map[String, Attribute])
        {
        val children = scala.collection.mutable.ListBuffer[Element]()
        val buf      = new StringBuilder
        }
    val stack = scala.collection.mutable.Stack[StackItem]()

    /**
     * from sax ContentHandler
     */
    def startElement(qname: String, attrs: Map[String, String]) =
        {
        val arr = qname.split(":")
        val name = if (arr.size > 1) arr(1) else qname
        val item = new StackItem(name, attrs.map(e=> e._1 -> Attribute(name=e._1, value=e._2)).toMap)
        stack.push(item)
        }

    /**
     * from sax ContentHandler
     */
    def characters(text: String) =
        stack.top.buf.append(text)

    /**
     * from sax ContentHandler
     */
    def endElement(name: String) =
        {
        val item = stack.pop
        val elem = Element(
            name       = item.name,
            attributes = item.attrs,
            children   = item.children.toList,
            value      = item.buf.toString.trim
            )
        if (!process(elem) && stack.size > 0)
            stack.top.children += elem
        }

    def depth = stack.size

    def process(elem: Element) : Boolean =
        {
        if (depth == 1)
            {
            println("elem:" + elem)
            true
            }
        else
            false
        }

    trait State
    case object TEXT        extends State
    case object ENTITY      extends State
    case object OPEN_TAG    extends State
    case object CLOSE_TAG   extends State
    case object START_TAG   extends State
    case object ATTR_NAME extends State
    case object ATTR_EQUAL  extends State
    case object ATTR_VALUE extends State
    case object QUOTE       extends State
    case object IN_TAG      extends State
    case object SINGLE_TAG  extends State
    case object COMMENT     extends State
    case object DONE        extends State
    case object DOCTYPE     extends State
    case object PRE         extends State
    case object CDATA       extends State

    private val sstack = scala.collection.mutable.Stack[State]()

    private def popState :  State =
        if (sstack.size > 0) sstack.pop else PRE

    private def pushState(s: State) =
        sstack.push(s)

    val EOF = (-1).toChar

    def get(ins: java.io.InputStream) : Char =
        {
        val ch = ins.read.toChar
        print(ch)
        ch
        }
    

    private var line    = 1
    private var col     = 0

    def error(s: String) =
        println("error : [%d:%d] : %s".format(line, col, s))
        
    private var aborted = false
    
    def abort =
        aborted = true

    def parse(ins: java.io.InputStream) : Boolean =
        {
        var state : State = PRE
        var quotec  = '"'
        var depth   = 0
        val sb      = new StringBuilder
        val etag    = new StringBuilder
        var tagName = ""
        var attrName  = ""
        var attrVal  = ""
        val attrs   = scala.collection.mutable.Map[String, String]()
        var eol     = false

        var ch = get(ins)

        var cont = true

        aborted = false
        while (!aborted && cont && ch != EOF)
            {
            if (ch == '\n' && eol) {
                eol = false
                //continue
              } else if (eol) {
                eol = false
              } else if (ch == '\n') {
                line += 1
                col=0
              } else if (ch == '\r') {
                eol = true
                ch = '\n'
                line += 1
                col=0
              } else {
                col += 1
              }

          state match
              {
              case DONE =>
                 cont = false

              case TEXT =>
                  if  (ch == '<')
                      {
                      pushState(state)
                      state = START_TAG
                      characters(sb.toString)
                      sb.clear
                      }
                  else if (ch == '&')
                      {
                      pushState(state)
                      state = ENTITY
                      etag.clear
                      }
                  else
                      {
                      sb.append(ch)
                      }

              case CLOSE_TAG =>
                  if (ch == '>')
                      {
                      state = popState
                      tagName = sb.toString
                      sb.clear
                      depth -= 1
                      if (depth <= 0)
                          state = DONE
                      endElement(tagName)
                      }
                  else
                      {
                      sb.append(ch)
                      }

              case CDATA =>
                  if (ch == '>' && sb.toString.endsWith("]]"))
                      {
                      sb.setLength(sb.length()-2)
                      characters(sb.toString)
                      sb.clear
                      state = popState
                      }
                  else
                      sb.append(ch)


              case COMMENT =>
                  if (ch == '>' && sb.toString.endsWith("--"))
                      {
                      sb.clear
                      state = popState
                      }
                  else
                      sb.append(ch)

              case PRE =>
                  if (ch == '<')
                      {
                      state = TEXT
                      pushState(state)
                      state = START_TAG
                      }

              case DOCTYPE =>
                  if (ch == '>')
                      {
                      state = popState
                      if (state == TEXT) state = PRE
                      }

              case START_TAG =>
                  state = popState
                  if (ch == '/')
                      {
                      pushState(state)
                      state = CLOSE_TAG
                      }
                  else if (ch == '?')
                      {
                      state = DOCTYPE
                      }
                  else
                      {
                      pushState(state)
                      state = OPEN_TAG
                      tagName = ""
                      attrs.clear
                      sb.append(ch)
                      }

              case ENTITY =>
                  if (ch == ';')
                      {
                      state = popState
                      val cent = etag.toString
                      etag.clear
                      if (cent == "lt")
                          sb.append('<')
                      else if (cent == "gt")
                          sb.append('>')
                      else if (cent == "amp")
                          sb.append('&')
                      else if (cent == "quot")
                          sb.append('"')
                      else if (cent == "apos")
                          sb.append('\'')
                      else if (cent.startsWith("#"))
                          sb.append(Integer.parseInt(cent.substring(1)).toString)
                      else
                          error("Unknown entity: &"+cent+"")
                      }
                  else
                      {
                      etag.append(ch)
                      }
                      
              case SINGLE_TAG =>
                  if (tagName.size == 0)
                      tagName = sb.toString
                  if (ch != '>')
                      error("Expected > for tag: <"+tagName+"/>")
                  startElement(tagName, attrs.toMap)
                  endElement(tagName)
                  if (depth==0)
                      {
                      cont = false
                      }
                  else
                      {
                      sb.clear
                      attrs.clear
                      tagName = ""
                      state = popState
                      }

              case OPEN_TAG =>
                  if (ch == '>')
                      {
                      tagName = sb.toString
                      sb.clear
                      depth += 1
                      startElement(tagName, attrs.toMap)
                      tagName = ""
                      attrs.clear
                      state = popState
                      }
                  else if (ch == '/')
                      {
                      state = SINGLE_TAG
                      }
                  else if (ch == '-' && sb.toString == "!-")
                      {
                      state = COMMENT
                      }
                  else if (ch == '[' && sb.toString == "![CDATA")
                      {
                      state = CDATA
                      sb.clear
                      }
                  else if(ch == 'E' && sb.toString == "!DOCTYP")
                      {
                      sb.clear
                      state = DOCTYPE
                      }
                  else if (ch.isWhitespace)
                      {
                      tagName = sb.toString
                      sb.clear
                      state = IN_TAG
                      }
                  else
                      {
                      sb.append(ch)
                      }

              case QUOTE =>
                  if (ch == quotec)
                      {
                      attrVal = sb.toString
                      sb.clear
                      attrs += attrName -> attrVal
                      state = IN_TAG
                      }
                  else if (" \r\n\u0009".indexOf(ch) >= 0)
                      {
                      sb.append(' ')
                      }
                  else if (ch == '&')
                      {
                      pushState(state)
                      state = ENTITY
                      etag.clear
                      }
                  else
                      {
                      sb.append(ch)
                      }

              case ATTR_VALUE =>
                  if (ch == '"' || ch == '\'')
                      {
                      quotec = ch
                      state = QUOTE
                      }
                  else if (ch.isWhitespace)
                      {
                      }
                  else
                      {
                      error("Error in attribute processing")
                      }

              case ATTR_NAME =>
                  if (ch.isWhitespace)
                      {
                      attrName = sb.toString
                      sb.clear
                      state = ATTR_EQUAL
                      }
                  else if (ch == '=')
                      {
                      attrName = sb.toString
                      sb.clear
                      state = ATTR_VALUE
                      }
                  else
                      {
                      sb.append(ch)
                      }

              case ATTR_EQUAL =>
                  if (ch == '=')
                     {
                     state = ATTR_VALUE
                     }
                 else if (ch.isWhitespace)
                     {
                     }
                 else
                     {
                     error("Error in attribute processing.")
                     }

              case IN_TAG =>
                  if (ch == '>')
                      {
                      state = popState
                      startElement(tagName, attrs.toMap)
                      depth += 1
                      tagName = ""
                      attrs.clear
                      }
                  else if(ch == '/')
                      {
                      state = SINGLE_TAG
                      }
                  else if (ch.isWhitespace)
                      {
                      }
                  else
                      {
                      state = ATTR_NAME
                      sb.append(ch)
                      }
              }//match

              ch = get(ins)

          }//while

          if (!aborted && state != DONE)
              {
              error("missing end tag: " + state)
              false
              }
          else
              true
      }//parse

}//XmppParser



object XmppParserTest
{
    def test =
        {
        val xml = "<root><a v=\"hello\"/><b v=\"world\"/><c/></root>"
        val p = new XmppParser
        p.parse(new java.io.ByteArrayInputStream(xml.getBytes))
        }

    def main(argv: Array[String]) : Unit =
        {
        test
        }
}




