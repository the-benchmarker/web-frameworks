val TargetJavaVersion = "25"

initialize := {
  val _ = initialize.value
  val actualJavaVersion = sys.props.getOrElse("java.specification.version", "unknown")
  if (actualJavaVersion != TargetJavaVersion)
    sys.error("Expected Java " + TargetJavaVersion + ", got " + actualJavaVersion)
}

lazy val root = (project in file("."))
  .enablePlugins(PlayScala)
  .settings(
    name := "server",
    organization := "com.example",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := "3.10.1",
    scalacOptions += ("-release:" + TargetJavaVersion),
    javacOptions ++= Seq("--release", TargetJavaVersion),
    libraryDependencies ++= Seq(
      guice
    )
  )
