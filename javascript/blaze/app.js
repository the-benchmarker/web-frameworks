const { Blaze, initializeServices } = require('@busy-hour/blaze');
const { serve } = require('@hono/node-server');
const path = require('node:path');

const app = new Blaze({});

initializeServices({
  app,
  path: path.resolve(__dirname, 'services'),
});

serve(app);
