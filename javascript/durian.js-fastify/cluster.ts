import { availableParallelism } from 'node:os';

const numCpus = availableParallelism();

for (let i = 0; i < numCpus; i++) {
  Bun.spawn(['bun', 'src', 'main.ts'], {
    stdio: ['inherit', 'inherit', 'inherit'],
    env: { ...process.env },
  });
}
