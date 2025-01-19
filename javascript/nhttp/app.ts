import nhttp from "@nhttp/nhttp";

const app = nhttp();

app.get('/', () => {
  return new Response();
});

app.get('/user/:id', (rev) => {
  return new Response(rev.params.id);
});

app.post('/user', () => {
  return new Response();
});

export default {fetch: app.handle };

