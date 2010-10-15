/**
 * Simple Scala IRC client lib
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
 
package pedro.net



import scala.actors.Actor
import scala.actors.Actor._
import scala.collection.mutable.ListBuffer

//########################################################################
//### class IrcEvent
//########################################################################
trait IrcEvent

case class IrcConnected(user:String, host:String) extends IrcEvent
case class IrcDisconnected(user:String, host:String) extends IrcEvent

case class IrcStatus(msg: String) extends IrcEvent
case class IrcChat(from:String, to:String, msg:String) extends IrcEvent
case class IrcMotd(msg:String) extends IrcEvent
case class IrcJoin(user:String, channel:String) extends IrcEvent
case class IrcPart(user:String, channel:String) extends IrcEvent
case class IrcNick(oldNick:String, newNick:String) extends IrcEvent
case class IrcQuit(nick:String, fullid: String, msg: String) extends IrcEvent
case class IrcNotice(msg:String) extends IrcEvent
case class IrcInfo(msg:String) extends IrcEvent
case class IrcMode(channel: String, mode:String) extends IrcEvent

case class IrcNameList(channel: String, names:List[String]) extends IrcEvent
case class IrcChanList(channels:List[String]) extends IrcEvent
case class IrcWhoList(channel: String, users:List[String]) extends IrcEvent

case class IrcCtcp(from:String, target: String, param:String) extends IrcEvent
case class IrcPing(from:String, target: String) extends IrcEvent
case class IrcAction(from:String, target: String, param:String) extends IrcEvent
case class IrcUrl(from:String, target: String, url:String) extends IrcEvent
case class IrcSound(from:String, target: String, name:String) extends IrcEvent
case class IrcVersion(from:String, target: String) extends IrcEvent




class IrcClient(
    val host: String    = "irc.freenode.net",
    val port: Int       = 6667,
    val name: String    = "noname",
    val nick: String    = "nonick",
    val channel: String = "",
    val observer: Actor = Actor.actor{},
    val debug: Boolean  = false
    )
{
    //########################################################################
    //# STANDARD   Irc   CHAT   DEFINITIONS
    //########################################################################
    /**
     * Reserve numerics 000-099 for server-client connections where the client
     * is local to the server. If any server is passed a numeric in this range
     * from another server then it is remapped to 100-199
     */
    val RPL_WELCOME          = 001
    val RPL_YOURHOST         = 002
    val RPL_CREATED          = 003
    val RPL_MYINFO           = 004
    
    /**
     * Errors are in the range from 400-599 currently and are grouped by what
     * commands they come from.
     */
    val ERR_NOSUCHNICK       = 401
    val ERR_NOSUCHSERVER     = 402
    val ERR_NOSUCHCHANNEL    = 403
    val ERR_CANNOTSENDTOCHAN = 404
    val ERR_TOOMANYCHANNELS  = 405
    val ERR_WASNOSUCHNICK    = 406
    val ERR_TOOMANYTARGETS   = 407
    val ERR_NOSUCHSERVICE    = 408
    val ERR_NOORIGIN         = 409
    
    val ERR_NORECIPIENT      = 411
    val ERR_NOTEXTTOSEND     = 412
    val ERR_NOTOPLEVEL       = 413
    val ERR_WILDTOPLEVEL     = 414
    
    val ERR_UNKNOWNCOMMAND   = 421
    val ERR_NOMOTD           = 422
    val ERR_NOADMININFO      = 423
    val ERR_FILEERROR        = 424
    
    val ERR_NONICKNAMEGIVEN  = 431
    val ERR_ERRONEUSNICKNAME = 432
    val ERR_NICKNAMEINUSE    = 433
    val ERR_SERVICENAMEINUSE = 434
    val ERR_SERVICECONFUSED  = 435
    val ERR_NICKCOLLISION    = 436
    
    val ERR_USERNOTINCHANNEL = 441
    val ERR_NOTONCHANNEL     = 442
    val ERR_USERONCHANNEL    = 443
    val ERR_NOLOGIN          = 444
    val ERR_SUMMONDISABLED   = 445
    val ERR_USERSDISABLED    = 446
    
    val ERR_NOTREGISTERED    = 451
    
    val ERR_NEEDMOREPARAMS   = 461
    val ERR_ALREADYREGISTRED = 462
    val ERR_NOPERMFORHOST    = 463
    val ERR_PASSWDMISMATCH   = 464
    val ERR_YOUREBANNEDCREEP = 465
    val ERR_YOUWILLBEBANNED  = 466
    val ERR_KEYSET           = 467
    
    val ERR_CHANNELISFULL    = 471
    val ERR_UNKNOWNMODE      = 472
    val ERR_INVITEONLYCHAN   = 473
    val ERR_BANNEDFROMCHAN   = 474
    val ERR_BADCHANNELKEY    = 475
    val ERR_BADCHANMASK      = 476
    
    val ERR_NOPRIVILEGES     = 481
    val ERR_CHANOPRIVSNEEDED = 482
    val ERR_CANTKILLSERVER   = 483
    
    val ERR_NOOPERHOST       = 491
    val ERR_NOSERVICEHOST    = 492
    
    val ERR_UMODEUNKNOWNFLAG = 501
    val ERR_USERSDONTMATCH   = 502
    
    /**
     * Numberic replies from server commands.
     * These are currently in the range 200-399.
     */
    val RPL_NONE             = 300
    val RPL_AWAY             = 301
    val RPL_USERHOST         = 302
    val RPL_ISON             = 303
    val RPL_TEXT             = 304
    val RPL_UNAWAY           = 305
    val RPL_NOWAWAY          = 306
    
    val RPL_WHOISUSER        = 311
    val RPL_WHOISSERVER      = 312
    val RPL_WHOISOPERATOR    = 313
    
    val RPL_WHOWASUSER       = 314
    /* rpl_endofwho below (315) */
    val RPL_ENDOFWHOWAS      = 369
    
    val RPL_WHOISCHANOP      = 316 /* redundant and not needed but reserved */
    val RPL_WHOISIDLE        = 317
    
    val RPL_ENDOFWHOIS       = 318
    val RPL_WHOISCHANNELS    = 319
    
    val RPL_LISTSTART        = 321
    val RPL_LIST             = 322
    val RPL_LISTEND          = 323
    val RPL_CHANNELMODEIS    = 324
    
    val RPL_NOTOPIC          = 331
    val RPL_TOPIC            = 332
    val RPL_TOPIC_DTG        = 333 //epoch time topic was set
    
    val RPL_INVITING         = 341
    val RPL_SUMMONING        = 342
    
    val RPL_VERSION          = 351
    
    val RPL_WHOREPLY         = 352
    val RPL_ENDOFWHO         = 315
    val RPL_NAMREPLY         = 353
    val RPL_ENDOFNAMES       = 366
    
    val RPL_KILLDONE         = 361
    val RPL_CLOSING          = 362
    val RPL_CLOSEEND         = 363
    val RPL_LINKS            = 364
    val RPL_ENDOFLINKS       = 365
    /* rpl_endofnames above (366) */
    val RPL_BANLIST          = 367
    val RPL_ENDOFBANLIST     = 368
    /* rpl_endofwhowas above (369) */
    
    val RPL_INFO             = 371
    val RPL_MOTD             = 372
    val RPL_INFOSTART        = 373
    val RPL_ENDOFINFO        = 374
    val RPL_MOTDSTART        = 375
    val RPL_ENDOFMOTD        = 376
    
    val RPL_YOUREOPER        = 381
    val RPL_REHASHING        = 382
    val RPL_YOURESERVICE     = 383
    val RPL_MYPORTIS         = 384
    val RPL_NOTOPERANYMORE   = 385
    
    val RPL_TIME             = 391
    val RPL_USERSSTART       = 392
    val RPL_USERS            = 393
    val RPL_ENDOFUSERS       = 394
    val RPL_NOUSERS          = 395
    
    val RPL_TRACELINK        = 200
    val RPL_TRACECONNECTING  = 201
    val RPL_TRACEHANDSHAKE   = 202
    val RPL_TRACEUNKNOWN     = 203
    val RPL_TRACEOPERATOR    = 204
    val RPL_TRACEUSER        = 205
    val RPL_TRACESERVER      = 206
    val RPL_TRACESERVICE     = 207
    val RPL_TRACENEWTYPE     = 208
    val RPL_TRACECLASS       = 209
    
    val RPL_STATSLINKINFO    = 211
    val RPL_STATSCOMMANDS    = 212
    val RPL_STATSCLINE       = 213
    val RPL_STATSNLINE       = 214
    val RPL_STATSILINE       = 215
    val RPL_STATSKLINE       = 216
    val RPL_STATSQLINE       = 217
    val RPL_STATSYLINE       = 218
    val RPL_ENDOFSTATS       = 219
    
    val RPL_UMODEIS          = 221
    
    val RPL_SERVICEINFO      = 231
    val RPL_ENDOFSERVICES    = 232
    val RPL_SERVICE          = 233
    val RPL_SERVLIST         = 234
    val RPL_SERVLISTEND      = 235
    
    val RPL_STATSLLINE       = 241
    val RPL_STATSUPTIME      = 242
    val RPL_STATSOLINE       = 243
    val RPL_STATSHLINE       = 244
    val RPL_STATSSLINE       = 245
    val RPL_STATSDEBUG       = 249
    
    val RPL_LUSERCLIENT      = 251
    val RPL_LUSEROP          = 252
    val RPL_LUSERUNKNOWN     = 253
    val RPL_LUSERCHANNELS    = 254
    val RPL_LUSERME          = 255
    val RPL_ADMINME          = 256
    val RPL_ADMINLOC1        = 257
    val RPL_ADMINLOC2        = 258
    val RPL_ADMINEMAIL       = 259
    
    val RPL_TRACELOG         = 261
    
    /* non-standard enums, user-initiated */
    val CMD_JOIN             = 1001
    val CMD_PART             = 1002
    val CMD_NICK             = 1003
    val CMD_PRIVMSG          = 1004
    val CMD_MODE             = 1005
    val CMD_QUIT             = 1006
    val CMD_NOTICE           = 1007


    /**
     * Public so it can be overridden
     */
    def trace(msg: String) =
        if (debug) pedro.log.trace("IrcClient: " + msg)

    /**
     * Public so it can be overridden
     */
    def error(msg: String) =
        pedro.log.error("IrcClient error: " + msg)
        
    def doNames(chanName: String) : Boolean =
        {
        if (!checkConnect) return false
        channel(chanName).names.clear
        send("NAMES " + chanName)
        }

    def doJoin(chanName: String) : Boolean =
        {
        if (!checkConnect) return false
        channel(chanName).names.clear
        send("JOIN " + chanName)
        }
        
    def doPart(chanName: String) : Boolean =
        {
        if (!checkConnect) return false
        send("PART " + chanName)
        }
        
    def doWho(query: String) : Boolean =
        {
        if (!checkConnect) return false
        whobuf.clear
        send("WHO " + query)
        }
        
    def doMsg(target: String, msg: String) : Boolean =
        {
        if (!checkConnect) return false
        send("PRIVMSG " + target + " :" + msg)
        }
        
    def doCtcp(target: String, cmd: String, msg: String = "") : Boolean =
        {
        if (!checkConnect) return false
        if (msg.size==0)
            doMsg(target, "\u0001" + cmd + "\u0001")
        else
            doMsg(target, "\u0001" + cmd + " " + msg + "\u0001")
        }
        
    def doAction(target: String, msg: String) : Boolean =
        {
        if (!checkConnect) return false
        doCtcp(target, "ACTION", msg)
        }

    def doVersion(target: String, msg: String) : Boolean =
        {
        if (!checkConnect) return false
        doCtcp(target, "VERSION", msg)
        }
        
    def motd = motdbuf.toString
    private val motdbuf = new StringBuilder

    def info = infobuf.toString
    private val infobuf = new StringBuilder

    def who = whobuf.toList
    private val whobuf  = ListBuffer[String]()

    def channelList = chanbuf.toList
    private val chanbuf = ListBuffer[String]()

    //per-channel info cache.  add fields as needed
    class IrcChannel(val name: String)
        {
        val names = scala.collection.mutable.HashSet[String]()
        def nameList =
            names.toList.sortWith((a,b) => a.toLowerCase < b.toLowerCase)
        }

    private val channelMap = scala.collection.mutable.Map[String, IrcChannel]()

    def channel(name: String) =
        channelMap.getOrElseUpdate(name, new IrcChannel(name))

    private var socket : Option[java.net.Socket]        = None
    private var outs   : Option[java.io.BufferedWriter] = None
    private var ins    : Option[java.io.BufferedReader] = None

    /**
     * Return whether we consider ourselves connected
     */         
    def isOpen =
        socket.isDefined && !socket.get.isClosed
        
    private def checkConnect =
        {
        if (!isOpen)
            {
            error("not connected")
            false
            }
        else
            true
        }

    private def doConnect : Boolean =
        {
        try
            {
            val sock = new java.net.Socket(host, port)
            sock.setSoTimeout(1500)
            socket = Some(sock)
            outs = Some(new java.io.BufferedWriter(new java.io.OutputStreamWriter(sock.getOutputStream)))
            ins  = Some(new java.io.BufferedReader(new java.io.InputStreamReader(sock.getInputStream)))
            actor(receiveLoop)
            send("USER scala * * :Scala Client")
            send("NICK " + nick)
            if (channel.length > 0)
                doJoin(channel)
            observer!IrcConnected(name, host)
            true
            }
        catch
            {
            case e:Exception => error("connect: " + e)
            doDisconnect
            false
            }
        }

    private def doDisconnect : Boolean =
        {
        receiveMore = false
        if (socket.isDefined)
            socket.get.close
        socket = None
        ins    = None
        outs   = None
        observer!IrcDisconnected(name, host)
        true
        }
        
    /**
     * Send a raw string to the server
     */         
    def send(str: String) : Boolean =
        {
        trace("send: '" + str + "'")
        if (!checkConnect) return false
        try
            { outs.get.write(str) ; outs.get.write("\n") ; outs.get.flush ; true }
        catch
            { case e: Exception => error("send: " + e) ; false }
        }
    
    private def readLine : Option[String] =
        {
        try
            { Option(ins.get.readLine) }
        catch
            {
            case e: java.net.SocketTimeoutException => ; Some("") // trace("listening")
            case e: Exception => error("send: " + e) ; None
            }
        }


    private var receiveMore = true

    private def receiveLoop =
        {
        receiveMore = true
        while (receiveMore && socket.isDefined)
            {
            val str = readLine
            if (str.isEmpty || (str.isDefined && str.get.length > 0 && !process(str.get)))
                receiveMore = false
            }
        doDisconnect
        }


    private var retryMore = true
    
    /**
     * Attempt to connect to the server.  Actually, this is a retry loop, and
     * the real connection is made in doConnect     
     */         
    def connect : Boolean =
        {
        actor
            {
            retryMore = true
            while (retryMore)
                {
                if (socket.isEmpty)
                    doConnect
                Thread.sleep(60000)
                }
            }
        true //Todo: what does this mean?
        }
    
    /**
     * Disconnect from the server.  Turn off the retry loop and call
     * doDisconnect
     */         
    def disconnect : Boolean =
        {
        retryMore = false
        doDisconnect
        true
        }


    def processCtcp(nick: String, target: String, param: String) =
        {
        val str = param.split("\u0001").last
        if (str.startsWith("ACTION "))
            observer!IrcAction(nick, target, str.substring(7))
        else if (str.startsWith("VERSION"))
            {
            observer!IrcVersion(nick, target)
            doVersion(nick, "ScalaNode:1.0:Scala")
            }
        else if (str.startsWith("URL "))
            observer!IrcUrl(nick, target, str.substring(4))
        }


    private val ircRegex =
        "^(?:[:@](\\S+)\\s+)?(?:(\\S+)\\s+)(?:([^:\\s]+)\\s+)*(?::?(.*))?$".r    //mine.
        //4 parts.  optional prefix, command, 0 or more middle params (as one thing), trailing param
        //[^:\s] means  "any char except a space or colon"
        //we could unroll the "0 or more" in the future if needed


    /**
     * Process a raw string from the server
     */         
    private def process(str: String) : Boolean =
        {
        trace("process: " + str)

        //first check for server commands
        if (str.length == 0 || str(0)!=':')
            {
            if (str.startsWith("PING"))
                {
                val words = str.split(" :")
                if (words.size<2 || words(1).size==0)
                    send("PONG " + host)
                else
                    send("PONG " + words(1))
                }
            return true
            }

        val ircRegex(from, cmd, target, param) = str
        trace("from: '"   + from + "'")
        trace("cmd: '"    + cmd + "'")
        trace("target: '" + target + "'")
        trace("param: '"  + param + "'")
        
        
        val cmdnr = cmd match
            {
            case "JOIN"    => CMD_JOIN
            case "PART"    => CMD_PART
            case "NICK"    => CMD_NICK
            case "PRIVMSG" => CMD_PRIVMSG
            case "MODE"    => CMD_MODE
            case "QUIT"    => CMD_QUIT
            case "NOTICE"  => CMD_NOTICE
            case _ => try
                          { cmd.toInt }
                      catch
                          { case _ => error("unknown command:" + cmd) ; return false }
            }
        
        val nick = from.split("!")(0)

        cmdnr match
            {
            case CMD_JOIN    =>
                val chan = param
                channel(chan).names.add(nick)
                observer!IrcNameList(chan, channel(chan).nameList) 
                observer!IrcJoin(from, chan)
            case CMD_PART    =>
                val chan = param
                channel(chan).names.remove(nick)
                observer!IrcNameList(chan, channel(chan).nameList) 
                observer!IrcPart(from, chan)
            case CMD_NICK    =>
                val chan = param
                channel(chan).names.remove(nick)
                channel(chan).names.add(param)
                observer!IrcNick(nick, param) 
                observer!IrcNameList(chan, channel(chan).nameList) 
            case CMD_PRIVMSG =>
                if (param(0) == '\01')
                    processCtcp(nick, target, param)
                else
                    observer!IrcChat(nick, target, param)
            case CMD_MODE    =>
                observer!IrcMode(from, target.split(" ").last)
            case CMD_QUIT    =>
                channelMap.valuesIterator.foreach(info=>
                    {
                    if (info.names.remove(nick))
                        observer!IrcNameList(info.name, info.nameList)
                    }) 
                observer!IrcQuit(nick, from, param)
            case CMD_NOTICE  =>
                observer!IrcNotice(param)

            case RPL_MOTDSTART =>
                motdbuf.clear
            case RPL_MOTD =>
                motdbuf.append(param + "\n")
            case RPL_ENDOFMOTD =>
                observer!IrcMotd(motd)
            case RPL_INFOSTART =>
                infobuf.clear
            case RPL_INFO =>
                infobuf.append(param + "\n")
            case RPL_ENDOFINFO =>
                observer!IrcInfo(info)
            case RPL_WHOREPLY =>
                param.split(" ").foreach(nm=> whobuf.append(nm))
            case RPL_ENDOFWHO =>
                observer!IrcWhoList(from, who)
                whobuf.clear
            case RPL_NAMREPLY =>
                val chan = target.split(" ").last // "nick #channel "
                param.split(" ").foreach(nm=> channel(chan).names.add(nm))
            case RPL_ENDOFNAMES =>
                val chan = target.split(" ").last // "nick @ #channel "
                observer!IrcNameList(chan, channel(chan).nameList) 
            case RPL_LISTSTART => /* CHANNELS */
                chanbuf.clear
            case RPL_LIST =>
                param.split(" ").foreach(ch=> chanbuf.append(ch))
            case RPL_LISTEND =>
                observer!IrcChanList(channelList)
            case _ =>
                observer!IrcStatus(from + "/" + target + "/" + param)
            }
        true
        }
}





object IrcClientTest
{
    def out(s: String) =
        println(s)

    def doTest =
        {
        val obs = actor
            {
            while (true)
                {
                receive
                    {
                    case IrcConnected(user, host) =>
                        out("connected: " + user + " : " + host)
                    case IrcDisconnected(user, host) =>
                        out("disconnected: " + user + " : " + host)
                    case IrcStatus(msg) =>
                        out("status:" + msg)
                    case IrcChat(from, to, msg) =>
                        out("from:" + from + " to:" + to + " msg:" + msg)
                    case IrcMotd(msg) =>
                        out("motd:" + msg)
                    case IrcJoin(nick, channel) =>
                        out("join:" + channel + " : " + nick)
                    case IrcPart(nick, channel) =>
                        out("part:" + channel + " : " + nick)
                    case IrcNick(oldNick, newNick) =>
                        out("nick: '" + oldNick + "' is now known as '" + newNick + "'")
                    case IrcQuit(user, fullid, msg) =>
                        out("quit: " + user + " (" + fullid + ") has left irc : " + msg)
                    case IrcNotice(msg) =>
                        out("notice: " + msg)
                    case IrcInfo(msg) =>
                        out("info: " + msg)
                    case IrcMode(channel, mode) =>
                        out("mode: " + channel + " : " + mode)
                    case IrcNameList(channel, names) =>
                        out("names:" + channel + " : " + names.mkString(","))
                    case IrcChanList(channels) =>
                        out("channels: " + channels.mkString(","))
                    case IrcWhoList(channel, users) =>
                        out("who: " + channel + " : " + users.mkString(","))
                    case IrcCtcp(from, to, msg) =>
                        out("from:"+from + " to:" + to + " msg:" + msg)
                    case IrcPing(from, to) =>
                        out("from:"+from + " to:" + to)
                    case IrcAction(from, to, msg) =>
                        out("from:"+from + " to:" + to + " msg:" + msg)
                    case IrcUrl(from, to, msg) =>
                        out("from:"+from + " to:" + to + " msg:" + msg)
                        java.awt.Desktop.getDesktop().browse(new java.net.URI(msg))
                    case IrcSound(from, to, msg) =>
                        out("from:"+from + " to:" + to + " msg:" + msg)
                    case IrcVersion(from, to) =>
                        out("from:"+from + " to:" + to)
                    }
                }
            }
        val cli = new IrcClient(debug=true, channel="#scala",observer=obs)
        cli.connect
        }

    def main(argv: Array[String]) =
        {
        doTest
        }

}


class IrcBot(configName: String = "ircbot.js")
{
    var host = "irc.freenode.net"
    var port = 6667
    var nick = "nonick"
    var channel = "#scala"

    def error(msg: String) =
        pedro.log.error("IrcBot error: " + msg)

    def trace(msg: String) =
        pedro.log.error("IrcBot: " + msg)

    def out(s: String) =
        println(s)

    val obs = actor
        {
        while (true)
            {
            receive
                {
                case IrcConnected(user, host) =>
                    out("connected: " + user + " : " + host)
                case IrcDisconnected(user, host) =>
                    out("disconnected: " + user + " : " + host)
                case IrcStatus(msg) =>
                    out("status:" + msg)
                case IrcChat(from, to, msg) =>
                    out("from:" + from + " to:" + to + " msg:" + msg)
                case IrcMotd(msg) =>
                    out("motd:" + msg)
                case IrcJoin(nick, channel) =>
                    out("join:" + channel + " : " + nick)
                case IrcPart(nick, channel) =>
                    out("part:" + channel + " : " + nick)
                case IrcNick(oldNick, newNick) =>
                    out("nick: '" + oldNick + "' is now known as '" + newNick + "'")
                case IrcQuit(user, fullid, msg) =>
                    out("quit: " + user + " (" + fullid + ") has left irc : " + msg)
                case IrcNotice(msg) =>
                    out("notice: " + msg)
                case IrcInfo(msg) =>
                    out("info: " + msg)
                case IrcMode(channel, mode) =>
                    out("mode: " + channel + " : " + mode)
                case IrcNameList(channel, names) =>
                    out("names:" + channel + " : " + names.mkString(","))
                case IrcChanList(channels) =>
                    out("channels: " + channels.mkString(","))
                case IrcWhoList(channel, users) =>
                    out("who: " + channel + " : " + users.mkString(","))
                case IrcCtcp(from, to, msg) =>
                    out("from:"+from + " to:" + to + " msg:" + msg)
                case IrcPing(from, to) =>
                    out("from:"+from + " to:" + to)
                case IrcAction(from, to, msg) =>
                    out("from:"+from + " to:" + to + " msg:" + msg)
                case IrcUrl(from, to, msg) =>
                    out("from:"+from + " to:" + to + " msg:" + msg)
                    java.awt.Desktop.getDesktop().browse(new java.net.URI(msg))
                case IrcSound(from, to, msg) =>
                    out("from:"+from + " to:" + to + " msg:" + msg)
                case IrcVersion(from, to) =>
                    out("from:"+from + " to:" + to)
                }
            }
        }

    def start : Boolean =
        {
        if (!getConfig)
            false
        else
            {
            trace("host: " + host)
            trace("port: " + port)
            trace("nick: " + nick)
            trace("channel: " + channel)
            val cli = new IrcClient(debug=true, host=host, port=port,
                   nick=nick, channel="#scala", observer=obs)
            cli.connect
            }
        }

    def getConfig : Boolean =
        {
        pedro.data.JsonParser.parseFile(configName) match
            {
            case pedro.data.JsonSuccess(js) =>
                host    = js("host")
                port    = js("port")
                nick    = js("nick")
                channel = js("channel")
                true
            case pedro.data.JsonError =>
                error("getConfig: could not load config file")
                false
            }
        }

}

object IrcBot
{
    def doTest =
        {
        val bot = new IrcBot
        bot.start
        }
    
    def main(argv: Array[String]) =
        doTest
}