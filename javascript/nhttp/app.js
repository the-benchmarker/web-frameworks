import nhttp from "nhttp-land";

const app = nhttp();

app.get("/", () => {
  return new Response("");
});

app.get("/user/:id", (rev) => {
  return new Response(rev.params.id);
});

app.post("/", () => {
  return new Response("");
});

app.listen(3000);
