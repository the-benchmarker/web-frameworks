plugins {
    id("io.micronaut.application") version "5.0.+"
    id("com.gradleup.shadow") version "9.+"
    id("io.micronaut.aot") version "5.0.+"
}

version = "1.0.0"
group = "benchmark.micronaut"

repositories {
    mavenCentral()
    gradlePluginPortal()
}

dependencies {
    annotationProcessor("io.micronaut:micronaut-http-validation")
    annotationProcessor("io.micronaut.serde:micronaut-serde-processor")
    
    // Core Micronaut dependencies
    implementation("io.micronaut:micronaut-runtime")
    implementation("io.micronaut:micronaut-http-server-netty")
    implementation("io.micronaut:micronaut-http-client")
    
    // JSON Serialization
    implementation("io.micronaut.serde:micronaut-serde-jackson")
    
    // Security
    implementation("io.micronaut.security:micronaut-security")
    implementation("io.micronaut.security:micronaut-security-annotations")
    
    // Health & Monitoring
    implementation("io.micronaut:micronaut-health")
    implementation("io.micronaut:micronaut-management")
    
    // Logging
    runtimeOnly("ch.qos.logback:logback-classic")
    
    // Performance
    compileOnly("io.micronaut:micronaut-aop")
}

application {
    mainClass.set("com.example.Application")
    applicationDefaultJvmArgs = listOf(
        "-Dmicronaut.environment=prod",
        "-Djava.security.egd=file:/dev/./urandom",
        "-XX:+UseContainerSupport",
        "-XX:MaxRAMPercentage=80.0"
    )
}

java {
    sourceCompatibility = JavaVersion.VERSION_21
    targetCompatibility = JavaVersion.VERSION_21
}

graalvmNative.toolchainDetection = false

micronaut {
    runtime("netty")
    processing {
        incremental.set(true)
        annotations.add("com.example.*")
    }
    aot {
        optimizeServiceLoading = true
        convertYamlToJava = true
        precomputeOperations = true
        cacheEnvironment = true
        optimizeClassLoading = true
        deduceEnvironment = true
        optimizeNetty = true
        replaceLogbackXml = true
    }
}

shadow {
    mergeServiceFiles()
    archiveClassifier.set("all")
    archiveBaseName.set("benchmark")
    archiveVersion.set("")
}

tasks.named<io.micronaut.gradle.docker.NativeImageDockerfile>("dockerfileNative") {
    jdkVersion = "25"
}

tasks.withType<JavaCompile> {
    options.encoding = "UTF-8"
    options.compilerArgs.addAll(listOf(
        "--parameters",
        "-Xlint:deprecation",
        "-Xlint:unchecked"
    ))
}
