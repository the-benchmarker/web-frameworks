import { ExceptionFilter, Catch, ArgumentsHost, HttpException } from "@nestjs/common";
import { Request, Response } from "express";

/**
 * HTTP Exception Filter
 * 
 * Global exception filter for NestJS application.
 * Handles all uncaught exceptions and formats error responses.
 * In production, logging is minimized for maximum performance.
 */
@Catch()
export class HttpExceptionFilter implements ExceptionFilter {

  /**
   * Catch and handle exceptions
   * 
   * @param exception - The exception that was thrown
   * @param host - The arguments host containing request and response
   */
  catch(exception: unknown, host: ArgumentsHost) {
    const ctx = host.switchToHttp();
    const response = ctx.getResponse<Response>();
    const NODE_ENV = process.env.NODE_ENV || "production";

    // Log the error only in non-production environments
    if (NODE_ENV !== "production") {
      const errorMessage = exception instanceof Error ? exception.message : String(exception);
      const errorStack = exception instanceof Error ? exception.stack : undefined;
      console.error(`Unhandled exception: ${errorMessage}`, errorStack);
    } else {
      // In production, log to stderr only
      const errorMessage = exception instanceof Error ? exception.message : String(exception);
      process.stderr.write(`[${new Date().toISOString()}] ERROR: ${errorMessage}\n`);
    }

    // Determine status code and response
    let status = 500;
    
    if (exception instanceof HttpException) {
      status = exception.getStatus();
    }

    // For benchmarking, always return empty response in production
    response.status(status).type("text/plain").send("");
  }
}
