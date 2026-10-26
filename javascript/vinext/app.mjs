import { startProdServer } from 'vinext/server/prod-server';

await startProdServer({ port: 3000, host: '0.0.0.0' });
