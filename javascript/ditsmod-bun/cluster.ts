import os from 'node:os';

const numCPUs = os.availableParallelism();
for (let i = 0; i < numCPUs; i++) {
  Bun.spawn(['bun', 'dist/main.js'], {
    stdio: ['inherit', 'inherit', 'inherit'],
    env: { ...process.env },
  });
}
