val SharafVersion = "0.24.0"

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
      "ba.sake" %% "sharaf-potateh" % SharafVersion
    )
  )
  .enablePlugins(JavaAppPackaging)
