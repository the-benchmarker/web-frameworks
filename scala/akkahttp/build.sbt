organization := "the.benchmarker"
name := "AkkaHttp"
version := "0.0.3"
scalaVersion := "2.13.2"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http"    % "[10.1.11,10.2)",
  "com.typesafe.akka" %% "akka-stream"  % "[2.6,2.7)"
)

enablePlugins(JavaAppPackaging)