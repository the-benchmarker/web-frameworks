const app = require('muneem')();

app.route({
  url: "/",
  to: function(req, res){
      res.end(200);
  }
})

app.route({
  url: "/user/:id",
  to: function(req, res){
      res.write(req.params.id);
      res.end();
  }
})

app.route({
  url: "/user",
  to: function(req, res){
      res.end(200);
  }
})

app.start(3000);