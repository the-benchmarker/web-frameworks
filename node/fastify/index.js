const fastify = require('fastify')()
 
const response = { 200: { type: 'string' } }
const pathWithoutParams = { schema: { response } }
const pathWithParams = {
  schema: {
    response,
    params: { id: 'string' }
  }
}

fastify.get('/', pathWithoutParams, function (req, res) {
  res.send('')
})

fastify.get('/user/:id', pathWithParams, function (req, res) {
  res.type('text/html').send(`${req.params.id}`)
})

fastify.post('/user', pathWithoutParams, function (req, res) {
  res.send('')
})
 
// Run the server!
fastify.listen(3000, function () {})
