from happyx import Server

app = Server("0.0.0.0", 3000)


@app.get("/")
def index():
    return ""


@app.get("/user/{id}")
def get_user(id: int):
    return f"{id}"


@app.post("/user")
def create_user():
    return ""


app.start()
