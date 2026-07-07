import { Module } from "@nestjs/common";
import { APP_FILTER } from "@nestjs/core";
import { AppController } from "./app.controller";
import { HttpExceptionFilter } from "./http-exception.filter";

/**
 * NestJS Benchmark Application Module
 * 
 * Main application module for the benchmark server.
 * Configures controllers, providers, and global filters.
 */
@Module({
  imports: [],
  controllers: [AppController],
  providers: [
    {
      provide: APP_FILTER,
      useClass: HttpExceptionFilter,
    },
  ],
})
export class AppModule {}
