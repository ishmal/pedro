# Pedro
Pedro is a collection of small Scala network utilities with an emphasis on
conciseness.  One of the goals is also reduction of dependencies. And we desire
clarity of code for the purpose of instruction.

# Some of the modules you will find in this library

## pedro.data.Xml
scala.xml's ability to embed literal xml in Scala code it admirable. But for a lot
or users, it is very difficult to generate XML dynamically.  This simple library
provides that.  It has immutable Element and Attribute types, for threadsafe sharing
among modules.  It also has projections similar to scala.xml's, for easy searching
and grabbing data from XML chunks.

## pedro.data.Json
This is another alternative to what is in the Scala runtime.  This parser generates
a tree of JsonValues, subclassed to their individual types.  It also has the ability
to do projections.

## pedro.net.OAuth
This is a thin client that provides OAuth1 and OAuth2 authentication for HTTP GET
and POST methods for whatever service you desire.  Examples are provided for 
working with Twitter and Facebook.

## pedro.net.IrcClient
This is a simple IRC client lib (everybody has one), based on a much older Java
client also written by me years ago, so it should be quite stable for your needs.
It can either be used as the core of a GUI client, or be used as a bot.  An
example of using as a bot is provided.

## pedro.net.SoapClient
This provides a super-simple SOAP client that wraps up am XML SOAP request, POSTs
it to the target server, and returns the result.
