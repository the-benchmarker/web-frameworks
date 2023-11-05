(function () {
  'use strict';

  const Hapi = require('@hapi/hapi');

  // Create a server with a host and port
  const server = Hapi.server({
    host: '0.0.0.0',
    port: 3000,
  });

  // Add the route
  server.route({
    method: 'GET',
    path: '/',
    handler: function (req, handler) {
      return '';
    },
  });

  server.route({
    method: 'GET',
    path: '/user/{id}',
    handler: function (req, handler) {
      return req.params.id;
    },
  });

  server.route({
    method: 'POST',
    path: '/user',
    handler: function (req, handler) {
      return '';
    },
  });

  // Start the server
  async function start() {
    try {
      await server.start();
    } catch (err) {
      console.log(err);
      process.exit(1);
    }

    console.log('Server running at:', server.info.uri);
  }

  start();
})();
