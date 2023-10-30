organization := "the-benchmarker"

name := "server"
scalaVersion := "3.3.1"

val AkkaVersion = "2.7.0"
val AkkaHttpVersion = "10.5.3"

lazy val root = (project
  .in(file("."))
  .settings(
    PlayKeys.akkaHttpScala3Artifacts := true,
    libraryDependencies ++= Seq(
      guice,
      "com.typesafe.play" %% "play-json" % "latest.integration",
      "com.typesafe.akka" %% "akka-http-core" % AkkaHttpVersion,
      "com.typesafe.akka" %% "akka-actor"                 % AkkaVersion,
      "com.typesafe.akka" %% "akka-actor-typed"           % AkkaVersion,
      "com.typesafe.akka" %% "akka-stream"                % AkkaVersion,
      "com.typesafe.akka" %% "akka-slf4j"                 % AkkaVersion,
      "com.typesafe.akka" %% "akka-serialization-jackson" % AkkaVersion,
    )
  )
  .enablePlugins(PlayScala)
  .enablePlugins(PlayAkkaHttpServer)
  .disablePlugins(PlayLayoutPlugin))
