var Foxify = require('foxify')

var app = new Foxify()

app.disable('X-Powered-By');

app.get('/', function (req, res) {
  res.send('')
})

app.get('/user/:id', function (req, res) {
  res.send(req.params.id)
})

app.post('/user', function (req, res) {
  res.send('')
})

app.start(function() {})
