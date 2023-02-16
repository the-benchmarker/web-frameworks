import archttp;

void main()
{
    auto app = new Archttp;

    app.get("/", (req, res) {
        res.send("");
    });

    app.get("/user/{id}", (req, res) {
        res.send(req.params["id"]);
    });

    app.post("/user", (req, res) {
        res.send("");
    });

    app.listen(3000);
}
