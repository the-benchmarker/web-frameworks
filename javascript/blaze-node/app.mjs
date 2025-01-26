import { Blaze } from '@busy-hour/blaze';

import coreService from './services/core.js';
import userService from './services/user.js';

const app = new Blaze();

app.import({
  autoStart: true,
  services: [coreService, userService],
});

app.serve(3000);
