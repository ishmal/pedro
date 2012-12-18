
name := "pedro"

version := "0.12.0"

organization := "org.pedro"

scalaVersion := "2.10.0-RC3"

libraryDependencies ++= Seq(
    "com.typesafe.akka"  % "akka-actor_2.10.0-RC1" % "2.1.0-RC1",
    "org.scala-lang"     % "scala-reflect"         % "2.10.0-RC1",
    "junit"              % "junit"                 % "4.10"      % "test",
    "org.scalatest"      % "scalatest_2.10.0-RC1"  % "2.0.M4-2.10.0-RC1-B1"    % "test",
    "com.h2database"     % "h2"                    % "1.3.168"   % "test",
    "org.apache.tomcat"  % "servlet-api"           % "6.0.35"    % "compile,test"
)

scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-language:dynamics",
    "-language:implicitConversions"
)


