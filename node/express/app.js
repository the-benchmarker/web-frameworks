var express = require('express')
var app = express()

app.get('/', function (req, res) {
  res.send('')
})

app.get('/user/:id', function (req, res) {
  res.send('')
})

app.post('/user/:id', function (req, res) {
  res.send('')
})

app.listen(3000, function() {
  console.log('listening on 3000')
})
