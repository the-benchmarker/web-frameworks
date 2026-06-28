import { ExceptionFilter, Catch, ArgumentsHost, HttpException } from "@nestjs/common";
import { Request, Response } from "express";
import { Logger } from "@nestjs/common";

/**
 * HTTP Exception Filter
 * 
 * Global exception filter for NestJS application.
 * Handles all uncaught exceptions and formats error responses.
 */
@Catch()
export class HttpExceptionFilter implements ExceptionFilter {
  private readonly logger = new Logger(HttpExceptionFilter.name);

  /**
   * Catch and handle exceptions
   * 
   * @param exception - The exception that was thrown
   * @param host - The arguments host containing request and response
   */
  catch(exception: unknown, host: ArgumentsHost) {
    const ctx = host.switchToHttp();
    const response = ctx.getResponse<Response>();
    const request = ctx.getRequest<Request>();

    // Log the error
    this.logger.error(
      `Unhandled exception: ${exception instanceof Error ? exception.message : String(exception)}`,
      exception instanceof Error ? exception.stack : undefined,
    );

    // Determine status code and response
    let status = 500;
    let message = "Internal Server Error";

    if (exception instanceof HttpException) {
      status = exception.getStatus();
      // For benchmarking, return empty message in production
      if (process.env.NODE_ENV === "production") {
        message = "";
      } else {
        message = exception.message || "Internal Server Error";
      }
    } else if (exception instanceof Error) {
      message = exception.message || "Internal Server Error";
    }

    // For benchmarking, return empty response in production
    if (process.env.NODE_ENV === "production") {
      message = "";
    }

    // Set response
    response.status(status).type("text/plain").send(message);
  }
}
