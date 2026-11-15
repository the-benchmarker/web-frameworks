import { spawn, Subprocess } from 'bun';

const cpus = navigator.hardwareConcurrency;
const buns = new Array<Subprocess>(cpus);

for (let i = 0; i < cpus; i++) {
  buns[i] = spawn({
    cmd: ['bun', './app.ts'],
    stdio: ['inherit', 'inherit', 'inherit'],
  });
}

function kill() {
  for (const bun of buns) {
    bun.kill();
  }
}

process.on('SIGINT', kill);
process.on('SIGTERM', kill);
process.on('exit', kill);
