import assert from 'node:assert/strict';
import { mkdtempSync, mkdirSync, readFileSync, rmSync, writeFileSync } from 'node:fs';
import { tmpdir } from 'node:os';
import { dirname, join } from 'node:path';
import { spawnSync } from 'node:child_process';
import test from 'node:test';

const script = new URL('./update-renovate-config-version.mjs', import.meta.url).pathname;

function run(configPath, config, upgrades) {
  const root = mkdtempSync(join(tmpdir(), 'renovate-framework-version-'));
  try {
    const target = join(root, configPath);
    mkdirSync(dirname(target), { recursive: true });
    writeFileSync(target, config);
    const dataFile = join(root, 'upgrades.json');
    writeFileSync(dataFile, JSON.stringify(upgrades));
    const result = spawnSync(process.execPath, [script], {
      cwd: root,
      env: { ...process.env, RENOVATE_POST_UPGRADE_COMMAND_DATA_FILE: dataFile },
      encoding: 'utf8',
    });
    assert.equal(result.status, 0, result.stderr);
    return readFileSync(target, 'utf8');
  } finally {
    rmSync(root, { recursive: true, force: true });
  }
}

test('updates a framework on a major or minor version change', () => {
  const config = 'framework:\n  website: nextjs.org\n  version: 16.3\n';
  assert.equal(run('javascript/nextjs/config.yaml', config, [{
    packageFile: 'javascript/nextjs/package.json', depName: 'next',
    currentVersion: '16.3.6', newVersion: '16.4.0',
  }]), config.replace('16.3', '16.4'));
});

test('ignores patches and unrelated dependencies, even at the same version', () => {
  const config = 'framework:\n  website: nextjs.org\n  version: 16.3\n';
  assert.equal(run('javascript/nextjs/config.yaml', config, [
    { packageFile: 'javascript/nextjs/package.json', depName: 'next', currentVersion: '16.3.0', newVersion: '16.3.9' },
    { packageFile: 'javascript/nextjs/package.json', depName: 'react', currentVersion: '16.3.0', newVersion: '16.4.0' },
  ]), config);
});

test('uses the framework website when the directory also names an adapter', () => {
  const config = 'framework:\n  website: fastapi.tiangolo.com\n  version: 0.141\n';
  assert.equal(run('python/weft-fastapi/config.yaml', config, [
    { packageFile: 'python/weft-fastapi/pyproject.toml', depName: 'weft', currentVersion: '0.141.0', newVersion: '0.142.0' },
    { packageFile: 'python/weft-fastapi/pyproject.toml', depName: 'fastapi', currentVersion: '0.141.0', newVersion: '0.142.0' },
  ]), config.replace('0.141', '0.142'));
});

test('matches a scoped framework package without matching its dependencies', () => {
  const config = 'framework:\n  github: Moro-JS/engine\n  version: 1.1\n';
  assert.equal(run('javascript/morojs-engine/config.yaml', config, [
    { packageFile: 'javascript/morojs-engine/package.json', depName: '@morojs/engine', currentVersion: '1.1.9', newVersion: '1.2.0' },
    { packageFile: 'javascript/morojs-engine/package.json', depName: 'unrelated', currentVersion: '1.1.0', newVersion: '1.3.0' },
  ]), config.replace('1.1', '1.2'));
});

test('recognizes Spring Boot parent updates', () => {
  const config = 'framework:\n  website: spring.io/projects/spring-boot\n  version: 4.1\n';
  assert.equal(run('java/spring/config.yaml', config, [{
    packageFile: 'java/spring/pom.xml', depName: 'org.springframework.boot:spring-boot-starter-parent',
    currentVersion: '4.1.0', newVersion: '4.2.0',
  }]), config.replace('4.1', '4.2'));
});

test('does not use another package from the framework vendor', () => {
  const config = 'framework:\n  website: symfony.com\n  version: 8.1\n';
  const packageFile = 'php/symfony/composer.json';
  assert.equal(run('php/symfony/config.yaml', config, [{
    packageFile, depName: 'symfony/console', currentVersion: '8.1.0', newVersion: '8.2.0',
  }]), config);
  assert.equal(run('php/symfony/config.yaml', config, [
    { packageFile, depName: 'symfony/console', currentVersion: '8.1.0', newVersion: '8.3.0' },
    { packageFile, depName: 'symfony/framework-bundle', currentVersion: '8.1.0', newVersion: '8.2.0' },
  ]), config.replace('8.1', '8.2'));
});
