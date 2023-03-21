import nhttp from "nhttp-land";

const app = nhttp();

app.get("/", () => {
  return new Response("");
});

app.get("/user/:id", ({ params }) => {
  return new Response(params.id);
});

app.post("/", () => {
  return new Response("");
});

app.listen(3000);
