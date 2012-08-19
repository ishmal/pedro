name := "Pedro"

version := "0.11.0"

organization := "org.pedro"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
    "junit"                % "junit"              % "4.10"      % "test",
    "org.scalatest"        % "scalatest_2.9.2"    % "2.0.M3"    % "test",
    "com.h2database"       % "h2"                 % "1.3.168"   % "test",
    "org.apache.tomcat"    % "tomcat-servlet-api" % "7.0.29"    % "compile,test"
)

scalacOptions += "-deprecation"

scalacOptions += "-unchecked"