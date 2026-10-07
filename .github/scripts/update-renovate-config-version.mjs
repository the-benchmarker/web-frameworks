import { existsSync, readFileSync, writeFileSync } from 'node:fs';
import { join } from 'node:path';

const dataFile = process.env.RENOVATE_POST_UPGRADE_COMMAND_DATA_FILE;
if (!dataFile) {
  throw new Error('RENOVATE_POST_UPGRADE_COMMAND_DATA_FILE is required');
}

const upgrades = JSON.parse(readFileSync(dataFile, 'utf8'));
const versionParts = (value) => /^(?:v)?(\d+)\.(\d+)(?:\.\d+)?$/.exec(value ?? '');
const normalize = (value) => value.toLowerCase().replace(/[^a-z0-9]/g, '');
const genericRepositoryNames = new Set(['core', 'engine', 'framework', 'server', 'web']);
const frameworkPackageNames = new Set(['core', 'framework', 'frameworkbundle']);

function frameworkNames(configPath, contents) {
  const names = new Set();
  const addName = (value) => {
    const name = normalize(value);
    if (!name) return;
    names.add(name);
    for (const suffix of ['framework', 'js']) {
      if (name.endsWith(suffix) && name.length > suffix.length + 2) {
        names.add(name.slice(0, -suffix.length));
      }
    }
  };
  const addRepository = (repository) => {
    const [owner, name] = repository.replace(/^https?:\/\//, '').replace(/^(?:github|gitee)\.com\//, '').split('/');
    if (!owner || !name) return;
    addName(`${owner}/${name}`);
    if (!genericRepositoryNames.has(name.toLowerCase())) addName(name);
  };
  const github = /^  github:\s*([^\s#]+)/m.exec(contents)?.[1];
  const gitee = /^  gitee:\s*([^\s#]+)/m.exec(contents)?.[1];
  const repository = github ?? gitee;
  if (repository) addRepository(repository);

  const website = /^  website:\s*([^\s#]+)/m.exec(contents)?.[1];
  if (website) {
    const url = new URL(website.includes('://') ? website : `https://${website}`);
    const host = url.hostname.split('.').filter((part) => !['www', 'docs'].includes(part))[0];
    const path = url.pathname.split('/').filter(Boolean);
    if (url.hostname === 'github.com' && path.length >= 2) {
      addRepository(`${path[0]}/${path[1]}`);
    } else if (url.hostname === 'github.com' && path.length === 1) {
      addName(path[0]);
    } else if (url.hostname === 'docs.microsoft.com') {
      addName('aspnetcore');
    } else {
      if (host) addName(host);
      if (['projects', 'package', 'p', 'pkg'].includes(path[0]) && path[1]) addName(path[1]);
      if (path[0] === 'en-us' && path[1]) addName(path[1]);
    }
  }

  // Spring Boot's Maven parent and Gradle plugin use different package names.
  if (['java/spring/', 'java/spring-webflux/', 'kotlin/spring/'].some((path) => configPath.startsWith(path))) {
    names.add('springboot');
  }
  return names;
}

function isFrameworkPackage(upgrade, names) {
  for (const value of [upgrade.depName, upgrade.packageName]) {
    if (!value) continue;
    const parts = value.replace(/^@/, '').split(/[/:]/);
    const whole = normalize(value);
    const packageName = normalize(parts.at(-1));
    if (names.has(whole) || names.has(packageName)) return true;
    if (parts.length > 1 && names.has(normalize(parts[0])) &&
        (frameworkPackageNames.has(packageName) ||
         [...names].some((name) => packageName === `${name}framework`))) return true;
    if (names.has('aspnetcore') && whole === 'microsoftaspnetcoreapp') return true;
    if (names.has('springboot') && [
      'orgspringframeworkboot',
      'springbootstarterparent',
      'orgspringframeworkbootspringbootstarterparent',
    ].includes(whole)) return true;
  }
  return false;
}

const candidates = new Map();

for (const upgrade of upgrades) {
  const { packageFile, currentVersion, newVersion } = upgrade;
  // Framework configuration lives at <language>/<framework>/config.yaml.
  const location = /^([^/.][^/]*)\/([^/.][^/]*)\//.exec(packageFile ?? '');
  const oldVersion = versionParts(currentVersion);
  const nextVersion = versionParts(newVersion);
  if (!location || !oldVersion || !nextVersion || packageFile.endsWith('/config.yaml')) continue;

  const configPath = join(location[1], location[2], 'config.yaml');
  if (!existsSync(configPath)) continue;

  const oldMajorMinor = `${oldVersion[1]}.${oldVersion[2]}`;
  const newMajorMinor = `${nextVersion[1]}.${nextVersion[2]}`;
  if (oldMajorMinor === newMajorMinor) continue;

  const updates = candidates.get(configPath) ?? [];
  updates.push({ oldMajorMinor, newMajorMinor, upgrade });
  candidates.set(configPath, updates);
}

for (const [configPath, updates] of candidates) {
  const contents = readFileSync(configPath, 'utf8');
  const names = frameworkNames(configPath, contents);
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
        .filter(({ oldMajorMinor, upgrade }) =>
          oldMajorMinor === `${currentMajorMinor[1]}.${currentMajorMinor[2]}` &&
          isFrameworkPackage(upgrade, names))
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
