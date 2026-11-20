import { createApp } from "@foal/core";
import { AppController } from "./app.controller";

async function main() {
  const app = await createApp(AppController);
  app.listen(3000, "0.0.0.0");
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
