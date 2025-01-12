import fast from "https://deno.land/x/fast@6.0.0-alpha.1/mod.ts";

const app = fast();

app.get("/", () => "");
app.get("/user/:id", (req) => req.params.id);
app.post("/user", () => "");

export default {
  reusePort: true,
  port: 3000,       // Port to run the server on
  fetch: app.serve, // Use the `app.serve` method to handle requests
};

