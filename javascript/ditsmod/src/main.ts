import { Application } from "@ditsmod/core";
import type { ServerOptions } from "node:http";

import { AppModule } from "./app/app.module.js";

const serverOptions: ServerOptions = { keepAlive: true, keepAliveTimeout: 0 };
const app = await Application.create(AppModule, { serverOptions });
export default app.server;
