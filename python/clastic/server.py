from clastic import Application, render_basic


def index():
    return ""


def create_user():
    return ""


def get_user(user_id):
    return user_id


routes = [
    ("/", index, render_basic),
    ("/user", create_user, render_basic),
    ("/user/<user_id>", get_user, render_basic),
]

app = Application(routes)
