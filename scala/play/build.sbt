val JavaVersion = "26"

initialize := {
  val _ = initialize.value
  val actualJavaVersion = sys.props.getOrElse("java.specification.version", "unknown")
  if (actualJavaVersion != JavaVersion)
    sys.error(s"Expected Java $JavaVersion, got $actualJavaVersion")
}

lazy val root = (project in file("."))
  .enablePlugins(PlayScala)
  .settings(
    name := "server",
    organization := "com.example",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.10.1",
    libraryDependencies ++= Seq(
      guice
    )
  )
