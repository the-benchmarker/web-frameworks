import { Server } from "happyx";


const app = new Server("0.0.0.0", 3000);


app.get("/", (req) => {
  return "";
});

app.get("/user/{id}", (req) => {
  return req.params.id;
});

app.post("/user", (req) => {
  return "";
});


app.start();
