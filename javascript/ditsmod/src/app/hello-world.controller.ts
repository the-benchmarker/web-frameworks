import { HTTP_BODY } from "@ditsmod/body-parser";
import {
  AnyObj,
  controller,
  inject,
  PATH_PARAMS,
  Res,
  route,
} from "@ditsmod/core";

@controller()
export class HelloWorldController {
  constructor(private res: Res) {}

  @route("GET")
  empty() {
    this.res.send();
  }

  @route("GET", "user/:id")
  userId(@inject(PATH_PARAMS) pathParams: AnyObj) {
    this.res.send(pathParams.id);
  }

  @route("POST", "user")
  postHello(@inject(HTTP_BODY) body: any) {
    this.res.send();
  }
}
