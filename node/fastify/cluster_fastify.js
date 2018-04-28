const cluster = require('cluster')
const http = require('http')
const fastify = require('fastify')
const numCPUs = require('os').cpus().length

if (cluster.isMaster) {
  for (let i = 0; i < numCPUs; i++) {
    cluster.fork()
  }
} else {
  const app = fastify()

  app.get('/', function(request, reply) {
    reply.send()
  })

  const opts = {
    schema: {
      response: {
        200: {
          type: 'string'
        }
      }
    }
  }

  app.get('/user/:id', opts, function(request, reply) {
    reply.send(request.params.id)
  })

  app.post('/user', function(request, reply) {
    reply.send()
  })

  app.listen(3000, function() {})
}
