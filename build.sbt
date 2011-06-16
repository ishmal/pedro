name := "Pedro"

version := "0.10"

organization := "org.pedro"

scalaVersion := "2.9.0"

libraryDependencies ++= Seq(
  "de.element34"         % "sbt-eclipsify"    % "0.7.1"     % "compile,test",
  "org.scalatest"        % "scalatest_2.9.0"  % "1.4.1"     % "compile,test",
  "com.h2database"       % "h2"               % "1.3.154"   % "compile,test",
  "org.apache.tomcat"    % "servlet-api"      % "6.0.29"    % "compile,test",
  "org.apache.cassandra" % "cassandra-all"    % "0.8.0-rc1" % "compile,test",
  "org.apache.cassandra" % "cassandra-thrift" % "0.8.0-rc1" % "compile,test",
  "org.apache.thrift"    % "libthrift"        % "0.6.1"     % "compile,test"
)

