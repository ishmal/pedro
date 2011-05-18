

import sbt._

/**
 * This is the sbt project definition for Pedro.  Manage dependencies and behavior here.
 */
class Pedro(info: ProjectInfo) extends DefaultProject(info)
{
  val scalatest  = "org.scalatest"        % "scalatest_2.9.0" % "1.4.1"   % "compile,test" intransitive()
  val h2         = "com.h2database"       % "h2"              % "1.3.154" % "compile,test" intransitive()
  val servletapi = "org.apache.tomcat"    % "servlet-api"     % "6.0.29"  % "compile,test" intransitive()
  val cassandra  = "org.apache.cassandra" % "cassandra-all"   % "0.7.5"   % "compile,test" intransitive()
}
