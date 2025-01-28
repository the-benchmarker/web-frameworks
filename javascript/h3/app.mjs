import { createServer } from "node:http"
import { createApp, eventHandler, toNodeListener, getRouterParams, createRouter } from "h3"

const router = createRouter()
    .get(
        "/user/:id",
        eventHandler((event) => event.context.params.id)
    )
    .post(
        "/user",
        eventHandler(() => "")
    )
    .get(
        "/",
        eventHandler(() => "")
    )


const app = createApp().use(router)
createServer(toNodeListener(app)).listen(3000);
