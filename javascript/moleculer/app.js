const { ServiceBroker } = require("moleculer");
const HTTPServer = require("moleculer-web");

const broker = new ServiceBroker({
  logger: false
});

broker.createService({
  name: "api",

  mixins: [HTTPServer],

  settings: {
    port: 3000,

    routes: [
      {
        aliases: {
          "GET /": "user.get",
          "POST /user": "user.post",
          "GET /user/:id": "user.getID"
        }
      }
    ]
  }
});

broker.createService({
  name: "user",

  actions: {
    get() {},
    post() {},
    getID(ctx) {
      return Number(ctx.params.id);
    }
  }
});

broker.start();
