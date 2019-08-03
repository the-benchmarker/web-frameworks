const nanoexpress = require('nanoexpress')

nanoexpress()
  .get('/', { isRaw: true, direct: true }, (res) => {
    res.end('')
  })
  .get('/user/:id', { isRaw: true, direct: true }, (res, req) => {
    res.end(req.getParameter(0))
  })
  .post('/user', { isRaw: true, direct: true }, (res) => {
    res.end('')
  })
  .listen(3000)
