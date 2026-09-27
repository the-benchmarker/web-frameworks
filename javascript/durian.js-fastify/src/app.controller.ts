import { Controller, Get, HttpCode, Param, Post } from "@nestjs/common";

@Controller()
export class AppController {
  @Get()
  getHello(): string {
    return "";
  }

  @Get("/user/:id")
  getUserId(@Param("id") id: string): string {
    return id;
  }

  @Post("/user")
  @HttpCode(200)
  postUser(): string {
    return "";
  }
}
