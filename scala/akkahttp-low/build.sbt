import sbtassembly.AssemblyPlugin.defaultShellScript

name := "AkkaHttp-Low"

version := "0.0.1"

scalaVersion := "2.12.2"

mainClass in Compile := Some("Main")

assemblyOption in assembly := (assemblyOption in assembly).value.copy(prependShellScript = Some(defaultShellScript))

assemblyJarName in assembly := "server_scala_akkahttp-low"

libraryDependencies ++= Seq(
	"com.typesafe.akka" %% "akka-http" % "10.0.6",
	"com.typesafe.akka" %% "akka-http-core" % "10.0.6"
)

