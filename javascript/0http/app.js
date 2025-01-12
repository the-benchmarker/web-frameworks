import cero from "0http"
const { router, server } = cero();

router.on('GET', '/', (req, res) => {
  res.end();
});

router.on('GET', '/user/:id', (req, res) => {
  res.end(req.params.id);
});

router.on('POST', '/user', (req, res) => {
  res.end();
});

export default server;
