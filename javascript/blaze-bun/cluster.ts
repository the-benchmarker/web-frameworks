import os from 'node:os';

for (let i = 0; i < os.availableParallelism(); i++) {
  Bun.spawn(['bun', 'app.ts'], {
    stdio: ['inherit', 'inherit', 'inherit'],
    env: { ...process.env },
  });
}
