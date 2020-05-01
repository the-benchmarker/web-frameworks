import sbtassembly.AssemblyPlugin.defaultShellScript

name := "AkkaHttp"

version := "0.0.2"

scalaVersion := "2.13.1"

mainClass in Compile := Some("Main")

assemblyOption in assembly := (assemblyOption in assembly).value
  .copy(prependShellScript = Some(defaultShellScript))

assemblyJarName in assembly := "server_scala_akkahttp"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-http" % "[10.1.11,10.2)",
  // "com.typesafe.akka" %% "akka-http-core" % "10.1.11"
  "com.typesafe.akka" %% "akka-stream" % "[2.6,2.7)"
)
