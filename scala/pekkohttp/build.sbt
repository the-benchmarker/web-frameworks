name := "server"
scalaVersion := "3.6.3"

val PekkoVersion = "1.1.3"
val PekkoHttpVersion = "[1.1,1.2]"
libraryDependencies ++= Seq(
  "org.apache.pekko" %% "pekko-actor-typed" % PekkoVersion,
  "org.apache.pekko" %% "pekko-stream" % PekkoVersion,
  "org.apache.pekko" %% "pekko-http" % PekkoHttpVersion
)
enablePlugins(JavaAppPackaging)
