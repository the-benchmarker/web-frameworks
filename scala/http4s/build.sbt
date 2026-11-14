val Http4sVersionRange = "[0.23,0.24)"

name := "server"

val JavaVersion = "25"

initialize := {
  val _ = initialize.value
  val actualJavaVersion = sys.props.getOrElse("java.specification.version", "unknown")
  if (actualJavaVersion != JavaVersion)
    sys.error(s"Expected Java $JavaVersion, got $actualJavaVersion")
}

scalaVersion := "3.10.1"

scalacOptions += s"-release:$JavaVersion"
javacOptions ++= Seq("--release", JavaVersion)

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-ember-server" % Http4sVersionRange,
      "org.http4s" %% "http4s-dsl" % Http4sVersionRange
    )
  )
  .enablePlugins(JavaAppPackaging)
