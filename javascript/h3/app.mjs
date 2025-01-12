import { createServer } from "node:http";
import {
  createApp,
  createRouter,
  eventHandler,
  getRouterParams,
  toNodeListener,
} from "h3";

const router = createRouter()
  .get(
    "/user/:id",
    eventHandler((event) => event.context.params.id),
  )
  .post(
    "/user",
    eventHandler(() => ""),
  )
  .get(
    "/",
    eventHandler(() => ""),
  );

const app = createApp().use(router);
export default createServer(toNodeListener(app));
