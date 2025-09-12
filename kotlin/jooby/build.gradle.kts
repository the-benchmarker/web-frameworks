plugins {
    val kotlinVersion: String by System.getProperties()
    val joobyVersion: String by System.getProperties()
    kotlin("jvm") version kotlinVersion
    id("io.jooby.run") version joobyVersion
    id("io.spring.dependency-management") version "1.1.0"
    id("com.google.osdetector") version "1.7.3"
    id("com.gradleup.shadow") version "8.3.7"
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
    implementation ("org.jetbrains.kotlin:kotlin-stdlib-jdk8:2.2.0")
    implementation ("io.jooby:jooby-logback")

    testImplementation("org.jetbrains.kotlin:kotlin-test")
    testImplementation("io.jooby:jooby-test")
    testImplementation("com.squareup.okhttp3:okhttp-jvm:5.1.0")
}


kotlin {
    jvmToolchain(21)

    compilerOptions {
        javaParameters = true
    }
}

tasks {
    test {
        useJUnitPlatform()
    }
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

