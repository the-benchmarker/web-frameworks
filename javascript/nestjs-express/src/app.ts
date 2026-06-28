import { Logger, Version } from "@nestjs/common";
import { NestFactory } from "@nestjs/core";
import { AppModule } from "./app.module";

/**
 * NestJS Benchmark Application Entry Point
 */
async function bootstrap() {
  const logger = new Logger("Bootstrap");
  
  // Get configuration from environment
  const port = parseInt(process.env.PORT || "3000", 10);
  const host = process.env.HOST || "0.0.0.0";

  // Create application
  const app = await NestFactory.create(AppModule);

  // Configure application for production
  if (process.env.NODE_ENV === "production") {
    app.enableShutdownHooks();
    app.disable("x-powered-by");
  }

  // Set global prefix if needed
  // app.setGlobalPrefix("api");

  // Start server
  await app.listen(port, host);
  
  logger.log(`NestJS benchmark server listening on ${host}:${port}`);
  logger.log(`NestJS version: ${Version()}`);
}

bootstrap().catch((error) => {
  Logger.error("Bootstrap error: " + error.message, "Bootstrap");
  process.exit(1);
});
