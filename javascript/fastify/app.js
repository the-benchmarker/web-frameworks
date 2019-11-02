const fastify = require("fastify");
const app = fastify();

app.get("/", function(request, reply) {
  reply.send();
});

const opts = {
  schema: {
    response: {
      200: {
        type: "string"
      }
    }
  }
};

app.get("/user/:id", opts, function(request, reply) {
  reply.send(request.params.id);
});

app.post("/user", function(request, reply) {
  reply.send();
});

app.listen(3000, "0.0.0.0", function() {});
