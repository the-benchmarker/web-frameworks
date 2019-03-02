import hug
from meinheld import patch

patch.patch_all()


hug.API(__name__).http.output_format = hug.output_format.text


@hug.get("/")
def index():
    return ""


@hug.get("/user/{id}")
def user_info(id):
    return str(id)


@hug.post("/user", methods=["POST"])
def user():
    return ""
