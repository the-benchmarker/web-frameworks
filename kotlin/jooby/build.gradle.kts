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
val mainAppClassName = "benchmark.AppKt"

dependencyManagement {
    imports {
        mavenBom("io.jooby:jooby-bom:$joobyVersion")
    }
}

dependencies {
    implementation ("io.jooby:jooby-netty")
    implementation ("io.jooby:jooby-kotlin")
    implementation(kotlin("stdlib"))
    implementation ("io.jooby:jooby-logback")
}


kotlin {
    jvmToolchain(25)

    compilerOptions {
        javaParameters = true
    }
}

tasks {
    shadowJar {
	archiveBaseName.set("server")
        archiveClassifier = null
        archiveVersion = null
        mergeServiceFiles()
        isZip64 = true
	manifest {
            attributes["Main-Class"] = mainAppClassName
        }
    }

    build {
        dependsOn(shadowJar)
    }

    joobyRun {
        mainClass = mainAppClassName
        restartExtensions = listOf("conf", "properties", "class")
        compileExtensions = listOf("java", "kt")
        port = 3000
    }
}

