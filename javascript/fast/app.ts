import fast from "https://deno.land/x/fast@6.0.0-alpha.1/mod.ts";

// Create the Fast app
const app = fast();

// Define routes
app.get("/", () => new Response(""));

app.post("/user", () => new Response("")
);

app.get("/user/:id", (req) => {
  return new Response(req.params.id);
});

export default { fetch: app.handle };
