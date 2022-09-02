name := "server"
scalaVersion := "3.2.0"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http"    % "[10.2,10.3)",
  "com.typesafe.akka" %% "akka-stream"  % "[2.6,2.7)"
)

enablePlugins(JavaAppPackaging)
