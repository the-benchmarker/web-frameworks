import hug
from meinheld import patch

patch.patch_all()


hug.API(__name__).http.output_format = hug.output_format.text


@hug.get("/")
def index():
    return ""


@hug.get("/user/{user_id}")
def user_info(user_id):
    return str(user_id)


@hug.post("/user", methods=["POST"])
def user():
    return ""
