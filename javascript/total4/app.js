require('total4')

ROUTE('GET /', function() {
  this.plain('Hello world!');
});

ROUTE('GET /user/{id}/', function(id) {
  this.plain(id);
});

ROUTE('POST /user/', function() {
  this.plain('');
});

F.http('release', { port: 3000, watcher: false });