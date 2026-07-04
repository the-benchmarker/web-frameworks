name := "server"
scalaVersion := "3.8.4"

val PekkoVersion = "1.6.0"
val PekkoHttpVersion = "[1.3,1.4]"
libraryDependencies ++= Seq(
  "org.apache.pekko" %% "pekko-actor-typed" % PekkoVersion,
  "org.apache.pekko" %% "pekko-stream" % PekkoVersion,
  "org.apache.pekko" %% "pekko-http" % PekkoHttpVersion
)
enablePlugins(JavaAppPackaging)
