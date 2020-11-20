name := "server"
scalaVersion := "2.13.3"
scalacOptions ++= Seq("-deprecation")

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http"    % "[10.2.0,10.3)",
  "com.typesafe.akka" %% "akka-stream"  % "[2.6.7,2.7)"
)

enablePlugins(JavaAppPackaging)
