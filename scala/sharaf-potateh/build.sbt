import scala.scalanative.build.Mode

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

enablePlugins(ScalaNativePlugin)

Compile / mainClass := Some("Main")
Compile / crossTarget := baseDirectory.value / "target" / "native"

scalacOptions ++= Seq("-release:" + TargetJavaVersion, "-Yretain-trees")
javacOptions ++= Seq("--release", TargetJavaVersion)

nativeConfig ~= { config =>
  config
    .withMode(Mode.releaseFull)
    .withMultithreading(true)
}

libraryDependencies += "ba.sake" %% "sharaf-potateh" % "0.24.0"
