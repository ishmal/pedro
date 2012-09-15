name := "Pedro"

version := "0.12.0"

organization := "org.pedro"

scalaVersion := "2.10.0-M7"

libraryDependencies ++= Seq(
    "org.scala-lang"     % "scala-actors"        % "2.10.0-M7",
    "org.scala-lang"     % "scala-reflect"       % "2.10.0-M7",
    "junit"              % "junit"               % "4.10"      % "test",
    "org.scalatest"      % "scalatest_2.10.0-M7" % "1.9-2.10.0-M7-B1"  % "test",
    "com.h2database"     % "h2"                  % "1.3.168"   % "test",
    "org.apache.tomcat"  % "servlet-api"         % "6.0.35"    % "compile,test"
)

scalacOptions ++= Seq(
    "-deprecation",
    "-unchecked",
    "-feature",
    "-language:dynamics",
    "-language:implicitConversions"
)


