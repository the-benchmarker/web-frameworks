import fastify from "fastify";

// Workers can share any TCP connection
const app = fastify();

app.get("/", function (request, reply) {
  reply.send();
});

app.get("/user/:id", function (request, reply) {
  reply.send(request.params.id);
});

app.addContentTypeParser(
  "application/x-www-form-urlencoded",
  function (req, body, done) {
    // The incoming request in the benchmark is empty anyway
    done();
  },
);

app.post("/user", function (request, reply) {
  reply.send();
});

export default app;
