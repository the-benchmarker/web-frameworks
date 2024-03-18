import http from '0http-bun'

const { router } = http()

router.get('/', (req) => {
  return new Response("")
})
router.get('/user/:id', (req) => {
  return new Response(req.params.id)
})
router.post('/user', (req) => {
  return new Response("")
})

Bun.serve({
  port: 3000,
  reusePort: true,
  fetch: router.fetch
})
