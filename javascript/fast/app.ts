import fast from "https://deno.land/x/fast@6.0.0-alpha.1/mod.ts";

const app = fast();

app.get("/", () => "");
app.get("/user/:id", (req) => req.params.id);
app.post("/user", () => "");

await app.serve({ port: 3000 });
