const NaturalRouter = require("natural-framework/lib/router");
// const path = require('path')

function createRoutes(router) {
  router
    .get("/", (_, response) => {
      response.end("");
    })
    .get("/user/:id", (request, response) => {
      response.end(request.params.id);
    })
    .post("/user", (request, response) => {
      response.end("");
    });
}

async function bootstrap() {
  const router = new NaturalRouter({
    // type: 'uws', // 'uws' or 'node'
    /* ssl: {
      key: path.join(__dirname, './security/cert.key'),
      cert: path.join(__dirname, './security/cert.pem')
    } */
  });
  try {
    createRoutes(router);
    await router.listen(3000);
    // console.log(`Listen http://localhost:${port}`)
  } catch (error) {
    // console.log('Error:', error)
  }
}

bootstrap();
