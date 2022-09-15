const os = require("os");
const cluster = require("cluster");
const fastify = require('fastify')({
    logger: false,
    disableRequestLogging: true
});

fastify.get("/", function (request, reply) {
  reply.send();
});

fastify.get("/user/:id", function (request, reply) {
  reply.send(request.params.id);
});

fastify.post("/user", function (request, reply) {
  reply.send();
});

const start = async () => {
    try {
        await fastify.listen({ port: 3000 });
    } catch (err) {
        process.exit(1);
    }
}

const clusterWorkerSize = os.cpus().length;

if (clusterWorkerSize > 1) {
    if (cluster.isMaster) {
        for (let i=0; i < clusterWorkerSize; i++) {
            cluster.fork();
        }

        cluster.on("exit", function(worker) {
            console.log("Worker", worker.id, " has exited.")
            cluster.fork();
        })
    } else {
        start();
    }
} else {
    start();
}
