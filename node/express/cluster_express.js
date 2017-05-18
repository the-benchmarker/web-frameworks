var cluster = require('cluster');
var express = require('express')
var numCPUs = require('os').cpus().length;


if (cluster.isMaster) {  
    for (var i = 0; i < numCPUs; i++) {
        // Create a worker
        cluster.fork();
    }
} else {
   var app = express()
   app.set('etag', false);

   app.get('/', function (req, res) {
     res.send('')
   })
   
   app.get('/user/:id', function (req, res) {
     res.send(req.params.id)
   })
   
   app.post('/user', function (req, res) {
     res.send('')
   })
   
   app.listen(3000, function() {})
}