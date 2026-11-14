val ZioHttpVersion = "3.11.6"

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
      "dev.zio" %% "zio-http" % ZioHttpVersion
    )
  )
  .enablePlugins(JavaAppPackaging)
