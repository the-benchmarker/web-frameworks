name := "server"
scalaVersion := "3.2.2"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http"    % "[10.4,10.5)",
  "com.typesafe.akka" %% "akka-stream"  % "[2.7,2.8)"
)

enablePlugins(JavaAppPackaging)
