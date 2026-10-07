import { existsSync, readFileSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';

const dataFile = process.env.RENOVATE_POST_UPGRADE_COMMAND_DATA_FILE;
if (!dataFile) {
  throw new Error('RENOVATE_POST_UPGRADE_COMMAND_DATA_FILE is required');
}

const upgrades = JSON.parse(readFileSync(dataFile, 'utf8'));
const versionParts = (value) => /^(?:v)?(\d+)\.(\d+)(?:\.\d+)?$/.exec(value ?? '');
const candidates = new Map();

for (const { packageFile, currentVersion, newVersion } of upgrades) {
  // Framework configuration lives at <language>/<framework>/config.yaml.
  const location = /^([^/.][^/]*)\/([^/.][^/]*)\//.exec(packageFile ?? '');
  const oldVersion = versionParts(currentVersion);
  const nextVersion = versionParts(newVersion);
  if (!location || !oldVersion || !nextVersion || packageFile.endsWith('/config.yaml')) continue;

  const configPath = join(location[1], location[2], 'config.yaml');
  if (!existsSync(configPath)) continue;

  const oldMajorMinor = `${oldVersion[1]}.${oldVersion[2]}`;
  const newMajorMinor = `${nextVersion[1]}.${nextVersion[2]}`;

  const updates = candidates.get(configPath) ?? [];
  updates.push({ oldMajorMinor, newMajorMinor });
  candidates.set(configPath, updates);
}

for (const [configPath, updates] of candidates) {
  const contents = readFileSync(configPath, 'utf8');
  const lines = contents.split(/(?<=\n)/);
  let inFramework = false;

  for (let index = 0; index < lines.length; index++) {
    const body = lines[index].replace(/\r?\n$/, '');
    if (/^framework:\s*(?:#.*)?$/.test(body)) {
      inFramework = true;
      continue;
    }
    if (inFramework && /^[^\s#]/.test(body)) break;
    if (!inFramework) continue;

    const match = /^(  version:\s*)(['"]?)(\d+\.\d+(?:\.\d+)?)(\2)(\s*(?:#.*)?)$/.exec(body);
    if (!match) continue;

    const currentMajorMinor = versionParts(match[3]);
    const desired = new Set(
      updates
        .filter(({ oldMajorMinor }) => oldMajorMinor === `${currentMajorMinor[1]}.${currentMajorMinor[2]}`)
        .map(({ newMajorMinor }) => newMajorMinor),
    );
    if (desired.size > 1) {
      console.warn(`Skipping ambiguous framework version in ${configPath}`);
    } else if (desired.size === 1) {
      const [version] = desired;
      if (match[3] !== version) {
        lines[index] = `${match[1]}${match[2]}${version}${match[4]}${match[5]}${lines[index].slice(body.length)}`;
        writeFileSync(configPath, lines.join(''));
        console.log(`Updated ${configPath} to ${version}`);
      }
    }
    break;
  }
}
