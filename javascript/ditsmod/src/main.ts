import { Application } from '@ditsmod/core';
import { ServerOptions } from 'node:http';

import { AppModule } from './app/app.module.js';

const serverOptions: ServerOptions = { keepAlive: true, keepAliveTimeout: 0 };
const app = await new Application().bootstrap(AppModule, { serverOptions });
app.server.listen(3000, "0.0.0.0");
