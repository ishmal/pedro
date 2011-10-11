name := "Pedro"

version := "0.10.1"

organization := "org.pedro"

scalaVersion := "2.9.1"

libraryDependencies ++= Seq(
  "org.scalatest"        % "scalatest_2.9.1"  % "1.6.1"     % "compile,test",
  "com.h2database"       % "h2"               % "1.3.154"   % "compile,test",
  "org.apache.tomcat"    % "servlet-api"      % "6.0.29"    % "compile,test",
  "org.apache.cassandra" % "cassandra-all"    % "0.8.0-rc1" % "compile,test",
  "org.apache.cassandra" % "cassandra-thrift" % "0.8.0-rc1" % "compile,test",
  "org.apache.thrift"    % "libthrift"        % "0.6.1"     % "compile,test"
)

