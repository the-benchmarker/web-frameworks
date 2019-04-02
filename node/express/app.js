var express = require('express')
var app = express()
app.set('etag', false)

app.get('/', function (req, res) {
  res.send('')
})

app.get('/user/:id(\\d+)', function (req, res) {
  res.send(req.params.id)
})

app.post('/user', function (req, res) {
  res.send('')
})

app.listen(3000, function () {})
