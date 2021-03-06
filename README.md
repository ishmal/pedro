# Pedro

Pedro is a collection of small Scala network utilities with an emphasis on
conciseness.  Users will notice that each of the code modules is VERY small.
We desire short, clear code for the purpose of instruction. One of the goals is
also reduction of dependencies. 

## Some of the modules you will find in this library

### pedro.net.servlet.RestServlet
This servlet makes it extremely easy to set up a REST API without needing any
big clumsy framework.  Simply define your resources, the actions you wish to
perform on them, and hook up to your business objest.  It couldn't be easier.

### pedro.data.Xml
scala.xml's ability to embed literal xml in Scala code it admirable. But for a lot
or users, it is very difficult to generate XML dynamically.  This simple library
provides that.  It has immutable Element and Attribute types, for threadsafe sharing
among modules.  It also has projections similar to scala.xml's, for easy searching
and grabbing data from XML chunks.

### pedro.data.Json
This is another alternative to what is in the Scala runtime.  This parser generates
a tree of JsonValues, subclassed to their individual types.  It also has the ability
to do projections.

### pedro.data.Properties
A simple wrapper of Java's Properties file, to allow Scala properties to be treated
as a Map[String,String].

### pedro.net.OAuth
This is a thin client that provides OAuth1 and OAuth2 authentication for HTTP GET
and POST methods for whatever service you desire.  Examples are provided for 
working with Twitter and Facebook.

### pedro.net.IrcClient
This is a simple IRC client lib (everybody has one), based on a much older Java
client also written by me years ago, so it should be quite stable for your needs.
It can either be used as the core of a GUI client, or be used as a bot.  An
example of using as a bot is provided.

### pedro.net.SoapClient
This provides a super-simple SOAP client that wraps up am XML SOAP request, POSTs
it to the target server, and returns the result.

### pedro.net.NtpClient
Again, small and simple is the rule.   How simple? 
val response = (new NtpClient("tick.usno.navy.mil")).ping
