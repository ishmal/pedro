
name := "pedro"

version := "0.12.0"

organization := "org.pedro"

scalaVersion := "2.10.0"

libraryDependencies ++= Seq(
    "com.typesafe.akka"  % "akka-actor_2.10" % "2.1.0",
    "org.scala-lang"     % "scala-reflect"   % "2.10.0",
    "junit"              % "junit"           % "4.11"     % "test",
    "org.scalatest"      % "scalatest_2.10"  % "2.0.M5b"  % "test",
    "com.h2database"     % "h2"              % "1.3.170"  % "test",
    "org.apache.tomcat"  % "servlet-api"     % "6.0.36"   % "compile, test"
)

scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-language:dynamics",
    "-language:implicitConversions"
)


