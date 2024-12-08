const fastify = require('fastify');

// Workers can share any TCP connection
const app = fastify();

app.get('/', function (request, reply) {
  reply.send();
});

app.get('/user/:id', function (request, reply) {
  reply.send(request.params.id);
});

app.post('/user', function (request, reply) {
  reply.send();
});

// Running Node.js will now share port 3000 between the workers:
app.listen({ port: 3000, host: '0.0.0.0' }, function (err, address) {
  if (err) {
    app.log.error(err);
    process.exit(1);
  }
  console.info(`Worker PID ${process.pid} is listening at ${address}`);
});
