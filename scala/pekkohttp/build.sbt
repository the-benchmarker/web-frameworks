name := "server"
scalaVersion := "3.9.0"

val PekkoVersion = "1.7.1"
val PekkoHttpVersion = "[1.4,1.5]"
libraryDependencies ++= Seq(
  "org.apache.pekko" %% "pekko-actor-typed" % PekkoVersion,
  "org.apache.pekko" %% "pekko-stream" % PekkoVersion,
  "org.apache.pekko" %% "pekko-http" % PekkoHttpVersion
)
enablePlugins(JavaAppPackaging)
