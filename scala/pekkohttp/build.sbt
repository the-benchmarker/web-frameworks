name := "server"

val JavaVersion = "26"

initialize := {
  val _ = initialize.value
  val actualJavaVersion = sys.props.getOrElse("java.specification.version", "unknown")
  if (actualJavaVersion != JavaVersion)
    sys.error(s"Expected Java $JavaVersion, got $actualJavaVersion")
}

scalaVersion := "3.10.1"

val PekkoVersion = "1.7.1"
val PekkoHttpVersion = "[2.0,2.1]"
libraryDependencies ++= Seq(
  "org.apache.pekko" %% "pekko-actor-typed" % PekkoVersion,
  "org.apache.pekko" %% "pekko-stream" % PekkoVersion,
  "org.apache.pekko" %% "pekko-http" % PekkoHttpVersion
)
enablePlugins(JavaAppPackaging)
