
name := "pedro"

version := "0.13.0"

organization := "org.pedro"

scalaVersion := "2.11.0"

libraryDependencies ++= Seq(
    "com.typesafe.akka"  % "akka-actor_2.11" % "2.3.2",
    "org.scala-lang"     % "scala-reflect"   % "2.11.0",
    "junit"              % "junit"           % "4.11"     % "test",
    "org.scalatest"      % "scalatest_2.11"  % "2.1.3"    % "test",
    "com.h2database"     % "h2"              % "1.4.177"  % "test",
    "org.apache.tomcat"  % "servlet-api"     % "6.0.39"   % "compile, test"
)

scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-language:dynamics",
    "-language:implicitConversions"
)


