<?php

use Cubex\Cubex;
use Cubex\Workerman\CubexWorker;
use App\Application;

$loader = require_once('vendor/autoload.php');

$worker = CubexWorker::create(
  __DIR__,
  $loader,
  function (Cubex $cubex) { return new Application($cubex); },
  'http://0.0.0.0:' . '3719',
)->setCount(4);

\Workerman\Worker::$pidFile = 'cubex.pid';
\Workerman\Worker::$logFile = 'cubex.log';

\Workerman\Worker::runAll();