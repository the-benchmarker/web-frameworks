name := "server"
scalaVersion := "3.7.3"
resolvers += "Akka library repository".at("https://repo.akka.io/maven")
val AkkaVersion = "2.10.5"
val AkkaHttpVersion = "10.7.2"
libraryDependencies ++= Seq(
    "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
    "com.typesafe.akka" %% "akka-stream" % AkkaVersion,
    "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion
)
enablePlugins(JavaAppPackaging)
