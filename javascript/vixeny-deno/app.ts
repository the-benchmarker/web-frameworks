import app from "./server.ts";

export default { fetch: await app.compose() }
