import { RestApplication } from "@loopback/rest";
import { AppController } from "./app.controller";

async function main() {
  const app = new RestApplication({
    rest: { port: 3000, host: "0.0.0.0" },
  });
  app.controller(AppController);
  await app.start();
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
