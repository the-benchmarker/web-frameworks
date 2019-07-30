const nanoexpress = require('nanoexpress')

nanoexpress()
  .get('/', { isRaw: true }, (req, res) => {
    res.end('')
  })
  .get('/user/:id', { isRaw: true }, (req, res) => {
    res.end(req.getParameter(0))
  })
  .post('/user', { isRaw: true }, (req, res) => {
    res.end('')
  })
  .listen(3000)
