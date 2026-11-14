name := "server"
scalaVersion := "3.10.0"

val PekkoVersion = "1.7.1"
val PekkoHttpVersion = "[2.0,2.1]"
libraryDependencies ++= Seq(
  "org.apache.pekko" %% "pekko-actor-typed" % PekkoVersion,
  "org.apache.pekko" %% "pekko-stream" % PekkoVersion,
  "org.apache.pekko" %% "pekko-http" % PekkoHttpVersion
)
enablePlugins(JavaAppPackaging)
