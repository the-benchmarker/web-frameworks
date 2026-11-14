import { get, param, post } from "@loopback/rest";

export class AppController {
  @get("/")
  index(): string {
    return "";
  }

  @get("/user/{id}")
  getUser(@param.path.string("id") id: string): string {
    return id;
  }

  @post("/user")
  postUser(): string {
    return "";
  }
}
