/**
 * 
 * Rexuws (Replace Express with uWebsocket) is a framework based on uWebsocket.js
 * It aims to be a drop-in, high speed replacement for express.
 * 
 * With uWebsockets.js clustering only properly works on Linux (not on WSL!).
 * (presumably dependend the OS's of SO_REUSEPORT support)
 * 
 */


const rex = require('rexuws').default;

const app = rex({
  useDefaultParser:true,
})

app.get("/", (req, res) => {
  res.send("");
})

app.get("/user/:id", function (req, res) {
  res.send(req.params.id);
});

app.post("/user", function (req, res) {
  res.send("");
});

app.listen(3000, function () {
  console.log("listening on port 3000");
});
