const { createServer } = require("node:http")
const { createApp, eventHandler, toNodeListener, getRouterParams, createRouter} = require("h3")

const router = createRouter()
    .get(
        "/user/:id",
        eventHandler((event) => { 
            return event.context.params.id
        })
    )
    .post(
        "/user",
        eventHandler(() => { 
            return ""
        })
    )
    .get(
        "/",
        eventHandler(() => { 
            return ""
        })
    )


const app = createApp().use(router)
createServer(toNodeListener(app)).listen(3000);
