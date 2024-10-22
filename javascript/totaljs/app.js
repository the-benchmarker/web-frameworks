require('total5')

ROUTE('GET /', function($) {
  $.text('Hello world!');
});

ROUTE('GET /user/{id}/', function($) {
  $.text($.params.id);
});

ROUTE('POST /user/', function($) {
  $.text('');
});

Total.run({ port: 3000, release: true, watcher: false });