import sbtassembly.AssemblyPlugin.defaultShellScript

name := "AkkaHttp"

version := "0.0.2"

scalaVersion := "2.12.7"

mainClass in Compile := Some("Main")

assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultShellScript))

assemblyJarName in assembly := "server_scala_akkahttp"

libraryDependencies ++= Seq(
	"com.typesafe.akka" %% "akka-http" % "10.1.5",
	"com.typesafe.akka" %% "akka-http-core" % "10.1.5",
	"com.typesafe.akka" % "akka-stream_2.12" % "2.5.12"
)
