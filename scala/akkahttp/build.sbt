name := "server"
scalaVersion := "2.13.10"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http"    % "[10.2,10.3)",
  "com.typesafe.akka" %% "akka-stream"  % "[2.6,2.7)"
)

enablePlugins(JavaAppPackaging)
