from micropie import App


class Root(App):

    async def index(self):
        return ""

    async def user(self, user_id=""):
        return user_id


app = Root()
