const cero = require('0http')
const low = require('0http/lib/server/low')

const { router, server } = cero({
  server: low()
})

router.on('GET', '/', (req, res) => {
  res.end()
})

router.on('GET', '/user/:id', (req, res, params) => {
  res.end(params.id)
})

router.on('POST', '/user', (req, res) => {
  res.end()
})

server.start(3000, socket => {})
