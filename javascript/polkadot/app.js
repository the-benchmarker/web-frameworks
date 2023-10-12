const polkadot = require('polkadot');

const Profile = /^\/user\/(?<id>[0-9]+)/i;

polkadot((req, res) => {
  if (req.method === 'GET') {
    if (req.path === '/') return res.end();

    const isProfile = Profile.exec(req.path);
    if (isProfile !== null) return res.end(isProfile.groups.id);
  } else if (req.method === 'POST' && req.path === '/user') {
    return res.end();
  }

  res.statusCode = 404;
  res.end();
}).listen(3000);
