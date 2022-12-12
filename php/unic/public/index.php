<?php
require(__DIR__.'/../vendor/autoload.php');

use Unic\App;

$app = new App();

$app->get('/', function($req, $res, $next) {
  $res->send('');
});

$app->start();
