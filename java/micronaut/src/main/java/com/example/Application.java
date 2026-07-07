package com.example;

import io.micronaut.runtime.Micronaut;

/**
 * Micronaut Benchmark Application Entry Point
 * 
 * <p>Production-grade Micronaut application with optimized startup
 * and production-ready configuration.</p>
 */
public class Application {

    /**
     * Main application entry point.
     * 
     * <p>Runs the Micronaut application with production environment settings.
     * AOT optimizations are enabled for better startup performance.</p>
     * 
     * @param args Command line arguments
     */
    public static void main(String[] args) {
        Micronaut.run(Application.class, args);
    }
}