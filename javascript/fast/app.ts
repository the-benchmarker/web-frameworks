import fast from "https://deno.land/x/fast@6.0.0-alpha.1/mod.ts";

// Create the Fast app
const app = fast();

// Define routes
app.get("/", (req, res) => {
  res.text("Welcome to Fast with Deno Serve!");
});

app.get("/json", (req, res) => {
  res.json({ message: "This is a JSON response from Fast!" });
});

app.get("/hello/:name", (req, res) => {
  const name = req.params.name;
  res.text(`Hello, ${name}!`);
});

export default { fetch: app.handle }
