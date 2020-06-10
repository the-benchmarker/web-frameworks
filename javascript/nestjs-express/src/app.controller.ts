import { Controller, Get, Param, Post } from "@nestjs/common";

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
  postUser(): string {
    return "";
  }
}
