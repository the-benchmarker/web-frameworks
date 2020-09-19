import oreoweb

api = Oreoweb()


app.route("/")
def home(req, resp):
    resp.text = "Hello, this is a home page."


@app.route("/about")
def about_page(req, resp):
    resp.text = "Hello, this is an about page."


@app.route("/{age:d}")
def tell_age(req, resp, age):
    resp.text = f"Your age is {age}"


@app.route("/{name:l}")
class GreetingHandler:
    def get(self, req, resp, name):
        resp.text = f"Hello, {name}"


@app.route("/show/template")
def handler_with_template(req, resp):
    resp.html = app.template("example.html", context={"title": "Awesome Framework", "body": "welcome to the future!"})


@app.route("/json")
def json_handler(req, resp):
    resp.json = {"this": "is JSON"}


@app.route("/custom")
def custom_response(req, resp):
    resp.body = b'any other body'
    resp.content_type = "text/plain"

if __name__ == "__main__":
    api.run()
