name := "server"
scalaVersion := "3.4.2"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "[10.6,10.7)",
  "com.typesafe.akka" %% "akka-stream" % "[2.7,2.8)"
)

enablePlugins(JavaAppPackaging)
