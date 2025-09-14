plugins {
    val kotlinVersion: String by System.getProperties()
    val joobyVersion: String by System.getProperties()
    kotlin("jvm") version kotlinVersion
    id("io.jooby.run") version joobyVersion
    id("io.spring.dependency-management") version "+"
    id("com.google.osdetector") version "+"
    id("com.gradleup.shadow") version "+"
}

repositories {
    mavenLocal()
    mavenCentral()
}

group "app"
version "1.0.0"

val kotlinVersion: String by System.getProperties()
val joobyVersion: String by System.getProperties()

val mainAppClassName = "app.AppKt"

dependencyManagement {
    imports {
        mavenBom("io.jooby:jooby-bom:$joobyVersion")
    }
}

dependencies {
    implementation ("io.jooby:jooby-netty")
    implementation ("io.jooby:jooby-kotlin")
    implementation ("org.jetbrains.kotlin:kotlin-stdlib-jdk8:$kotlinVersion")
    implementation ("io.jooby:jooby-logback")
}


kotlin {
    jvmToolchain(21)

    compilerOptions {
        javaParameters = true
    }
}

tasks {
    shadowJar {
        mergeServiceFiles()
    }

    joobyRun {
        mainClass = mainAppClassName
        restartExtensions = listOf("conf", "properties", "class")
        compileExtensions = listOf("java", "kt")
        port = 8080
    }
}

