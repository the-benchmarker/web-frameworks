val ZioHttpVersion = "3.12.0"

name := "server"

val TargetJavaVersion = "27"

initialize := {
  val _ = initialize.value
  val actualJavaVersion = sys.props.getOrElse("java.specification.version", "unknown")
  require(
    actualJavaVersion == TargetJavaVersion,
    s"Expected Java $TargetJavaVersion, got $actualJavaVersion"
  )
}

scalaVersion := "3.10.1"

scalacOptions += ("-release:" + TargetJavaVersion)
javacOptions ++= Seq("--release", TargetJavaVersion)

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio-http" % ZioHttpVersion
    )
  )
  .enablePlugins(JavaAppPackaging)
