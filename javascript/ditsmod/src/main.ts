import { Application } from '@ditsmod/core';
import { AppModule } from './app/app.module.js';

const app = await new Application().bootstrap(AppModule);
app.server.listen(3000, '0.0.0.0');
