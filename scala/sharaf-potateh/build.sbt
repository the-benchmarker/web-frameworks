ThisBuild / scalaVersion := "3.10.0"

lazy val root = (project in file("."))
  .settings(
    name := "server",
    libraryDependencies += "ba.sake" %% "sharaf-potateh" % "0.23.0",
    assembly / mainClass := Some("Main"),
    assembly / assemblyOutputPath := baseDirectory.value / "target" / "app.jar",
    assembly / assemblyMergeStrategy := {
      case PathList("META-INF", "services", _*) => MergeStrategy.concat
      case PathList("META-INF", _*)             => MergeStrategy.discard
      case _                                    => MergeStrategy.first
    }
  )
