plugins {
	id("org.springframework.boot") version "3.5.+"
	id("io.spring.dependency-management") version "+"
	kotlin("jvm") version "2.2.+"
	kotlin("plugin.spring") version "2.2.+"
}

group = "benchmark.spring-boot"
version = "0.0.1"

java {
	toolchain {
		languageVersion = JavaLanguageVersion.of(21)
	}
}

repositories {
	mavenCentral()
}

dependencies {
	implementation("org.springframework.boot:spring-boot-starter-web")
	implementation("com.fasterxml.jackson.module:jackson-module-kotlin")
	implementation("org.jetbrains.kotlin:kotlin-reflect")
}

tasks.bootJar {
	archiveFileName.set("server.jar")
}
