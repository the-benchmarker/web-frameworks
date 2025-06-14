import { Blaze } from "@busy-hour/blaze";

import coreService from "./services/core";
import userService from "./services/user";

const app = new Blaze();

app.import({
  autoStart: true,
  services: [coreService, userService],
});

Bun.serve({
  fetch: app.fetch,
  reusePort: true,
  port: 3000,
});
