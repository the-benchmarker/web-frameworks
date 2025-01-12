const HyperExpress = require("hyper-express");

// Create a new instance of HyperExpress HTTP
const port = 3000;
const app = new HyperExpress.Server();

// GET "/" => 200 with empty body
app.get("/", (request, response) => {
  response.send("");
});

// GET "/user/:id" => 200 with "id" as body
app.get("/user/:id", (request, response) => {
  response.send(request.path_parameters.id);
});

// POST "/user" => 200 with empty body
app.post("/user", (request, response) => {
  response.send("");
});

// Start the server on port 3000
app.listen(port);
