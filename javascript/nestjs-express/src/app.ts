import { Logger, Version } from "@nestjs/common";
import { NestFactory } from "@nestjs/core";
import { AppModule } from "./app.module";

/**
 * NestJS Benchmark Application Entry Point
 * 
 * Production-grade NestJS application with optimized startup and configuration.
 * Follows best practices including:
 * - Disabled debug logging
 * - Security best practices
 * - Performance optimization
 * - Graceful shutdown
 */
async function bootstrap() {
  // Production Configuration
  const port = parseInt(process.env.PORT || "3000", 10);
  const host = process.env.HOST || "0.0.0.0";
  const NODE_ENV = process.env.NODE_ENV || "production";

  // Create application
  const app = await NestFactory.create(AppModule, {
    // Disable logger in production for maximum performance
    logger: NODE_ENV === "production" ? false : undefined,
    // Disable automatic validation for benchmarking
    // Disable versioning for benchmarking
  });

  // Configure application for production
  app.enableShutdownHooks();
  app.disable("x-powered-by");
  
  // Security settings
  app.setGlobalPrefix("");
  
  // Performance settings
  // Increase payload size limit for benchmarking
  app.useBodyParser("json", { limit: "10mb" });
  app.useBodyParser("urlencoded", { extended: true, limit: "10mb" });

  // Start server
  await app.listen(port, host);
  
  // Only log startup in non-production environments
  if (NODE_ENV !== "production") {
    Logger.log(`NestJS benchmark server listening on ${host}:${port}`);
    Logger.log(`NestJS version: ${Version()}`);
  }
}

bootstrap().catch((error) => {
  // In production, log to stderr only
  if (process.env.NODE_ENV === "production") {
    process.stderr.write(`[${new Date().toISOString()}] ERROR: ${error.message}\n`);
  } else {
    Logger.error("Bootstrap error: " + error.message, "Bootstrap");
  }
  process.exit(1);
});
