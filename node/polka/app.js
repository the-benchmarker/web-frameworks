const polka = require('polka')

polka()
  .get('/', (req, res) => {
    res.end('')
  })
  .get('/user/:id', (req, res) => {
    const id = req.params.id
    if (parseInt(id, 10) * 0 !== 0) {
      res.statusCode = 404
      res.end()
    } else {
      res.end(id)
    }
  })
  .post('/user', (req, res) => {
    res.end('')
  })
  .listen(3000)
