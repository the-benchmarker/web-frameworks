var { Yume } = require('yume-server');

var yume = new Yume();
var PORT = process.env.PORT || 3000;

yume.get('/', function (req, res) {
  res.end('');
});

yume.get('/user/:id', function (req, res) {
  res.end(req.getParams().id);
});

yume.post('/user', function (_req, res) {
  res.end('');
});

yume.listen(PORT, () => {
  console.log(`>started @${PORT}`);
});
