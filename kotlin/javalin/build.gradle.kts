plugins {
    kotlin("jvm") version "2.4.+"
    application
    id("com.gradleup.shadow") version "+"
}

application {
    mainClass = "benchmark.javalin.BenchKt"
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("io.javalin:javalin:7.2.+")
    implementation("org.slf4j:slf4j-simple:2.0.+")
}

tasks {
    shadowJar {
        archiveBaseName.set("server")
        archiveClassifier = null
        archiveVersion = null
        mergeServiceFiles()
        isZip64 = true
    }

    build {
        dependsOn(shadowJar)
    }
}
