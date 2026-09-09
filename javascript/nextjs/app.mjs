import { createServer } from 'node:http';
import next from 'next';

const app = next({ dev: false });
await app.prepare();

createServer(app.getRequestHandler()).listen(3000, '0.0.0.0');
