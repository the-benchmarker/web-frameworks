name := "server"
scalaVersion := "3.2.2"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http"    % "[10.5,10.6)",
  "com.typesafe.akka" %% "akka-stream"  % "[2.8,2.9)"
)

enablePlugins(JavaAppPackaging)
