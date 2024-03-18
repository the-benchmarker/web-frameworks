const http = require('0http-bun')
const { router } = http({
  port: 3000
})

router.get('/', (req, res) => {
  return new Response()
})
router.get('/:id', async (req) => {
  return new Response(req.params.id)
})
router.post('/', async (req) => {
  return new Response()
})

export default router
