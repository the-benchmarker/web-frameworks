name := "server"
scalaVersion := "2.13.2"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http"    % "[10.1.11,10.2)",
  "com.typesafe.akka" %% "akka-stream"  % "[2.5,2.6)"
)

enablePlugins(JavaAppPackaging)
