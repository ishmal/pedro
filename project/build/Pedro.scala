

import sbt._

class Pedro(info: ProjectInfo) extends DefaultProject(info)
{
  val scalatest = "org.scalatest" % "scalatest_2.9.0" % "1.4.1" % "compile,test"
  val h2 = "com.h2database" % "h2" % "1.3.146" % "compile,test"
  val servletapi = "org.apache.tomcat" % "servlet-api" % "6.0.29" % "compile,test"
  val cassandra = "org.apache.cassandra" % "cassandra-all" % "0.7.0" % "compile,test"
}
