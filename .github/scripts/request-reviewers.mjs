const token = process.env.GITHUB_TOKEN;
const [owner, repository] = (process.env.GITHUB_REPOSITORY ?? '').split('/');
const pullNumber = process.env.PR_NUMBER;
const baseSha = process.env.PR_BASE_SHA;
const pullAuthor = process.env.PR_AUTHOR?.toLowerCase();
const apiUrl = process.env.GITHUB_API_URL ?? 'https://api.github.com';

if (!token || !owner || !repository || !pullNumber || !baseSha) {
  throw new Error('Missing GitHub Actions context required to request reviewers');
}

async function githubRequest(path, options = {}) {
  const response = await fetch(`${apiUrl}/repos/${owner}/${repository}/${path}`, {
    ...options,
    headers: {
      Accept: 'application/vnd.github+json',
      Authorization: `Bearer ${token}`,
      'X-GitHub-Api-Version': '2022-11-28',
      ...(options.body ? { 'Content-Type': 'application/json' } : {}),
      ...options.headers,
    },
  });

  if (!response.ok) {
    const detail = await response.text();
    throw new Error(`GitHub API ${options.method ?? 'GET'} ${path} failed (${response.status}): ${detail}`);
  }

  return response.status === 204 ? null : response.json();
}

async function listChangedFiles() {
  const files = [];

  for (let page = 1; ; page += 1) {
    const batch = await githubRequest(`pulls/${pullNumber}/files?per_page=100&page=${page}`);
    files.push(...batch);
    if (batch.length < 100) return files;
  }
}

function configPathsFor(changedPath) {
  const parts = changedPath.split('/');
  if (parts.length < 2 || !parts[0] || parts[0].startsWith('.')) return [];

  const paths = new Set([`${parts[0]}/config.yaml`]);
  if (parts.length >= 3 && parts[1]) paths.add(`${parts[0]}/${parts[1]}/config.yaml`);
  return [...paths];
}

function parseReviewers(yaml) {
  const lines = yaml.split(/\r?\n/);
  const index = lines.findIndex((line) => /^reviewers\s*:/.test(line));
  if (index < 0) return [];

  const header = lines[index].match(/^reviewers\s*:\s*(.*?)\s*(?:#.*)?$/)?.[1] ?? '';
  if (header.startsWith('[') && header.endsWith(']')) {
    return header.slice(1, -1).split(',').map(parseReviewer).filter(Boolean);
  }
  if (header) return [];

  const reviewers = [];
  for (const line of lines.slice(index + 1)) {
    if (!line.trim() || /^\s*#/.test(line)) continue;
    if (!/^\s/.test(line)) break;

    const item = line.match(/^\s+-\s*(.*?)\s*$/);
    if (!item) break;
    const reviewer = parseReviewer(item[1]);
    if (reviewer) reviewers.push(reviewer);
  }
  return reviewers;
}

function parseReviewer(value) {
  const withoutComment = value.replace(/\s+#.*$/, '').trim();
  const unquoted = withoutComment.replace(/^(?:'([^']*)'|"([^"]*)")$/, '$1$2').trim();
  const login = unquoted.replace(/^@/, '');
  return /^[A-Za-z0-9](?:[A-Za-z0-9-]{0,37})$/.test(login) ? login : null;
}

async function reviewersForConfig(path) {
  const encodedPath = path.split('/').map(encodeURIComponent).join('/');
  const response = await fetch(`${apiUrl}/repos/${owner}/${repository}/contents/${encodedPath}?ref=${encodeURIComponent(baseSha)}`, {
    headers: {
      Accept: 'application/vnd.github+json',
      Authorization: `Bearer ${token}`,
      'X-GitHub-Api-Version': '2022-11-28',
    },
  });

  if (response.status === 404) return [];
  if (!response.ok) {
    const detail = await response.text();
    throw new Error(`Could not read ${path} from the base revision (${response.status}): ${detail}`);
  }

  const file = await response.json();
  if (file.type !== 'file' || file.encoding !== 'base64') return [];
  return parseReviewers(Buffer.from(file.content, 'base64').toString('utf8'));
}

async function requestReviewers(reviewers) {
  const requested = new Map();
  for (const login of reviewers) {
    if (login.toLowerCase() !== pullAuthor) requested.set(login.toLowerCase(), login);
  }
  if (requested.size === 0) return;

  const pull = await githubRequest(`pulls/${pullNumber}`);
  for (const reviewer of pull.requested_reviewers ?? []) {
    requested.delete(reviewer.login.toLowerCase());
  }

  const logins = [...requested.values()];
  if (logins.length === 0) {
    console.log('All configured reviewers are already requested or authored this pull request.');
    return;
  }

  // GitHub accepts at most 15 reviewers per request.
  for (let offset = 0; offset < logins.length; offset += 15) {
    const batch = logins.slice(offset, offset + 15);
    const path = `pulls/${pullNumber}/requested_reviewers`;
    const response = await fetch(`${apiUrl}/repos/${owner}/${repository}/${path}`, {
      method: 'POST',
      headers: {
        Accept: 'application/vnd.github+json',
        Authorization: `Bearer ${token}`,
        'Content-Type': 'application/json',
        'X-GitHub-Api-Version': '2022-11-28',
      },
      body: JSON.stringify({ reviewers: batch }),
    });

    if (response.ok) {
      console.log(`Requested review from: ${batch.join(', ')}`);
      continue;
    }

    // A single account without repository access should not block other requests.
    const detail = await response.text();
    if (batch.length === 1) {
      console.warn(`Could not request ${batch[0]} as a reviewer (${response.status}): ${detail}`);
      continue;
    }

    if (response.status !== 422) {
      console.warn(`Could not request reviewer batch (${response.status}): ${detail}`);
      continue;
    }

    console.warn(`Could not request reviewer batch (${response.status}); retrying accounts individually: ${detail}`);
    for (const login of batch) {
      const individual = await fetch(`${apiUrl}/repos/${owner}/${repository}/${path}`, {
        method: 'POST',
        headers: {
          Accept: 'application/vnd.github+json',
          Authorization: `Bearer ${token}`,
          'Content-Type': 'application/json',
          'X-GitHub-Api-Version': '2022-11-28',
        },
        body: JSON.stringify({ reviewers: [login] }),
      });
      if (individual.ok) {
        console.log(`Requested review from: ${login}`);
      } else {
        console.warn(`Could not request ${login} as a reviewer (${individual.status}): ${await individual.text()}`);
      }
    }
  }
}

const changedFiles = await listChangedFiles();
const configPaths = new Set(changedFiles.flatMap(({ filename }) => configPathsFor(filename)));
const reviewers = new Set();

for (const path of configPaths) {
  for (const reviewer of await reviewersForConfig(path)) reviewers.add(reviewer);
}

console.log(`Found ${reviewers.size} configured reviewer(s) across ${configPaths.size} related config(s).`);
await requestReviewers([...reviewers]);
