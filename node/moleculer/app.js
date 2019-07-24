const { ServiceBroker } = require("moleculer");
const HTTPServer = require("moleculer-web");

const broker = new ServiceBroker();

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
    get(ctx) {
      return;
    },
    post(ctx) {
      return;
    },
    getID(ctx) {
      return ctx.params.id;
    }
  }
});

broker.start();
