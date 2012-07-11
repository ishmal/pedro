name := "Pedro"

version := "0.10.3"

organization := "org.pedro"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
  "org.scalatest"        % "scalatest_2.9.2"    % "2.0.M1"    % "compile,test",
  "com.h2database"       % "h2"                 % "1.3.167"   % "compile,test",
  "org.apache.tomcat"    % "tomcat-servlet-api" % "7.0.29"    % "compile,test"
)

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"