name := "server"
scalaVersion := "3.4.1"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "10.5.2",
  "com.typesafe.akka" %% "akka-stream" % "[2.7,2.8)"
)

enablePlugins(JavaAppPackaging)
