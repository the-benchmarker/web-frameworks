// brahma-firelight v1.5.18+ now supports true multi-core ⚡⚡

// import { availableParallelism } from 'node:os';

// const numCpus = availableParallelism();

// for (let i = 0; i < numCpus; i++) {
//   Bun.spawn(['bun', 'app.ts'], {
//     stdio: ['inherit', 'inherit', 'inherit'],
//     env: { ...process.env },
//   });
// }

// disabled for now
import './app';
