from fastapi import FastAPI

app = FastAPI()


@app.get("/")
async def index():
    return ""


@app.get("/user/{id}")
async def get_user(id: int):
    return id


@app.post("/user")
async def create_user():
    return ""
