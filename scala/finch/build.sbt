name := "finch"

scalaVersion := "2.13.3"

scalafmtOnCompile := true

libraryDependencies ++= Seq(
  "com.github.finagle" %% "finchx-core" % "0.33.0-SNAPSHOT"
)

assemblyMergeStrategy in assembly := {
  case "module-info.class" => MergeStrategy.discard
  case x if x.contains("io.netty.versions.properties") => MergeStrategy.discard
  case x =>
    val oldStrategy = (assemblyMergeStrategy in assembly).value
    oldStrategy(x)
}
