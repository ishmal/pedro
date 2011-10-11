/**
 * Simple CSV parser and writer
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


package pedro.data



class CsvReader extends pedro.util.Logged
{
    /**
     * Public, so that it can be overridden to redirect msgs
     */         
    def error(pos: Int, str: String) : Unit =
        error("CsvReader error (" + pos + "): " + str)

    private val EOF = (-1).asInstanceOf[Char]

    private var parsebuf = ""
    
    private val hexMatcher    = "[0-9a-fA-F]{4}".r 
    
    private def get(pos: Int) = 
        try
            { parsebuf(pos) }
        catch
            { case e: IndexOutOfBoundsException => EOF }
    
    private def skipwhite(pos: Int) : Int =
        {
        var p = pos
        while (p < parsebuf.length && parsebuf(p).isWhitespace)
            p += 1
        p
        }

    private def matchReg(pos: Int, regex:scala.util.matching.Regex) = 
        regex.findPrefixOf(parsebuf.substring(pos))

    private def quotedString(pos: Int) : (String, Int) =
        {
        val buf = new StringBuilder
        var p = pos
        while (p < parsebuf.size)
            {
            val ch = get(p)
            p += 1
            ch match
                { //escapes.  this is similar to JSON strings
                case '\\' =>
                    {
                    val ch2 = get(p)
                    p += 1
                    ch2 match 
                        { 
                        case '\\' => buf += '\\'
                        case '/'  => buf += '/'
                        case '"'  => buf += '"'
                        case 'f'  => buf += '\f'
                        case 'b'  => buf += '\b'
                        case 't'  => buf += '\t'
                        case 'r'  => buf += '\r'
                        case 'n'  => buf += '\n'
                        case 'u'  =>
                            {
                            val res = matchReg(p, hexMatcher)
                            if (res.isDefined)
                                {
                                buf += Integer.parseInt(res.get,16).asInstanceOf[Char]
                                p += 4
                                }
                            else
                                {
                                error(p, "bad unicode value in string")
                                return ("", -1)
                                }
                            }
                        case _ =>
                            {
                            error(p, "unknown escape in string")
                            return ("", -1)
                            }
                        } // ch2
                    }
                case '"' => return (buf.toString, p)
                case _   => buf += ch
                }//ch
            }
        error(p, "unterminated string")
        ("", -1)
        }


    /**
     * Parse line for CSV values.  Return empty list on failure
     */         
    def parse(s: String) : Seq[String] =
        {
        val xs = scala.collection.mutable.ListBuffer[String]()
        parsebuf = s
        val len = s.size
        var pos = 0
        var moreLine = true
        while (pos < len && moreLine)
            {
            pos = skipwhite(pos)
            if (get(pos) == '"')
                {
                val ret = quotedString(pos+1)
                if (ret._2<0)
                    return xs.toList
                else
                    {
                    xs += ret._1
                    pos = ret._2
                    }
                }
            else
                {
                val buf = new StringBuilder
                var moreTerm = true
                while (pos < len && moreTerm)
                    {
                    val ch = get(pos)
                    if (ch == EOF || ch == ',')
                        {
                        moreTerm = false
                        xs += buf.toString.trim
                        }
                    else
                        {
                        buf.append(ch)
                        pos += 1
                        }
                    }
                }
            pos = skipwhite(pos)
            val ch = get(pos)
            if (ch != ',')
                moreLine = false
            else
                pos += 1
            }
        if (pos != len)
            error(pos, "expected end-of-line or comma")
        xs.toList
        }

    /**
     * Parse a file for CSV lines.   Call f() for each line
     */         
    def parseFile(fname: String)(f: (Seq[String]) => Unit) =
        {
        try
            {
            scala.io.Source.fromFile(fname).getLines().foreach(s=> f(parse(s)))
            }
        catch
            {
            case e:Exception => error("parseFile: " + e)
            }
        }
}


class CsvWriter extends pedro.util.Logged
{
    private val hex = "0123456789abcdef".toCharArray
    
    private val badchars = Set(' ', '"', ',', '\\', '\f', '\b', '\t', '\r', '\n')
    
    private def needsQuotes(s:String) : Boolean =
        s.exists(ch=> badchars.contains(ch) || ch < 32 || ch > 127)

    /**
     * Output a CSV string  with all of the proper escapes
     */       
    private def writeString(buf: StringBuilder, ins: String) =
        {
        if (!needsQuotes(ins))
            buf.append(ins)
        else
            {
            buf.append('"')
            ins.foreach(ch=>
                {
                if (ch == '\\')                buf.append("\\\\")
                else if (ch == '\f')           buf.append("\\f")
                else if (ch == '\b')           buf.append("\\b")
                else if (ch == '\t')           buf.append("\\t")
                else if (ch == '\r')           buf.append("\\r")
                else if (ch == '\n')           buf.append("\\n")
                else if (ch >= 32 && ch < 127) buf.append(ch)
                else buf.append("\\u").
                     append(hex((ch >> 12)&0xf)).append(hex((ch >>  8)&0xf)).
                     append(hex((ch >>  4)&0xf)).append(hex((ch      )&0xf))
                })
            buf.append('"')
            }
        }

    private def writeLine(buf: StringBuilder, line:Seq[Any]) =
        {
        var comma = ""
        line.foreach(f =>
            {
            buf.append(comma)
            f match
                {
                case s: String => writeString(buf, s)
                case v => buf.append(v.toString)
                }
            comma = ","
            })
        buf.append("\n")        
        }

    def toString(lines: Seq[Seq[Any]]) : String =
        {
        val buf = new StringBuilder
        lines.foreach(a=> writeLine(buf, a))
        buf.toString
        }

    def writeFile(fname: String, lines: Seq[Seq[Any]]) : Boolean =
        {
        try
            {
            val out = new java.io.FileWriter(fname)
            out.write(toString(lines))
            out.close
            true
            }
        catch
            {
            case e:Exception => error("file write failed:" + e)
                false
            }
        }
}


/**
 * Utility object for parsing/serializing CSV data
 */ 
object Csv
{
    /**
     * Parse line for CSV values.  Return empty list on failure
     */         
    def parse(s:String) : Seq[String] =
        (new CsvReader).parse(s)

    /**
     * Parse a file for CSV lines.   Call f() for each line
     */         
    def parseFile(fname: String)(f: (Seq[String]) => Unit = {a:Seq[String]=>} ) =
        (new CsvReader).parseFile(fname)(f)
        
    /**
     * Convert a list of lists of data to a CSV string
     */         
    def toString(lines: Seq[Seq[Any]]) : String =
        (new CsvWriter).toString(lines)

    /**
     * Write a list of lists of data to a CSV file
     */         
    def writeFile(fname: String, lines: Seq[Seq[Any]]) : Boolean =
        (new CsvWriter).writeFile(fname, lines)
    

    /*
    val line = "the quick,\"brown, fox jumps\",over,\"the\",,\"lazy dog\""
    
    def doTest =
        {
        parse(line).foreach(println)
        parseFile("test.csv"){ xs => xs.foreach(println) }
        }
    
    def main(argv: Array[String]) =
        {
        doTest
        }
    */

}