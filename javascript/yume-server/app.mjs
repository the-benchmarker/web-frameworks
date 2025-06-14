import { Yume } from "yume-server";

const app = new Yume();
const PORT = 3000;

app.get('/', function (req, res) {
  res.end('');
});

app.get('/user/:id', function (req, res) {
  res.end(req.getParams().id);
});

app.post('/user', function (_req, res) {
  res.end('');
})

app.listen(PORT, () => {
  console.log(`> started @${PORT}`);
});

