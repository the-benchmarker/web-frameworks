val CaskVersion = "[0.11,0.12)"

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

run / fork := true

lazy val root = (project in file("."))
  .settings(
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "cask" % CaskVersion
    )
  )
  .enablePlugins(JavaAppPackaging)
