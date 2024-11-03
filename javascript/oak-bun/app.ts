import { Application, Router } from "@oak/oak";


const app = new Application();

const route = new Router();

route.get("/", (ctx) => {
    ctx.response.body = ''
})

route.get("/user/:id", (ctx) => {
    ctx.response.body = ctx.params.id
})

route.get("/user", (ctx) => {
    ctx.response.body = ''
})

app.use(route.routes());

app.serve({
  port: 3000,
        reusePort: true
})
