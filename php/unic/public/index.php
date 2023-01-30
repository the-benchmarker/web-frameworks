<?php
require(__DIR__.'/../vendor/autoload.php');

use Unic\App;
use Swoole\Http\Server;

$app = new App();
$server = new Server('0.0.0.0', 3000);

$app->get('/', function($req, $res) {
  $res->send('');
});

$app->get('/user/{id}', function($req, $res) {
  $res->send($req->params->id);
});

$app->post('/user', function($req, $res) {
  $res->send('');
});

$app->useOpenSwooleServer($server);
$app->start();
