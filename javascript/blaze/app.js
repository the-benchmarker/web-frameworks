const { Blaze } = require('@busy-hour/blaze');
const path = require('node:path');

const app = new Blaze({});

app.load({
  autoStart: true,
  path: path.resolve(__dirname, 'services'),
});

app.serve(3000);
