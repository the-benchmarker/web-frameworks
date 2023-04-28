import uWS from "uWebSockets.js";

uWS
  .App()
  .get("/", (res, req) => {
    res.end();
  })
  .get("/user/:id", (res, req) => {
    res.end(req.getParameter(0));
  })
  .post("/user", (res, req) => {
    res.end()
  })
  .any("/*", (res, req) => {
    res.writeStatus("404").end();
  })
  .listen(3000, () => {
    console.log("Server started on port 3000");
  });
