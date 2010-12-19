

import sbt._

class Pedro(info: ProjectInfo) extends DefaultProject(info)
{
  val scalatest = "org.scalatest" % "scalatest" % "1.2"
  val h2 = "com.h2database" % "h2" % "1.3.146"
  val servletapi = "org.apache.tomcat" % "servlet-api" % "6.0.29"
}
