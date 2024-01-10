<?php

use App\Application;
use Cubex\Cubex;
use Cubex\Workerman\CubexWorker;

$loader = require_once(dirname(__DIR__) . '/vendor/autoload.php');

$worker = CubexWorker::create(
  __DIR__,
  $loader,
  function (Cubex $cubex) { return new Application($cubex); },
  'http://0.0.0.0:3000',
);

$worker->setCount(shell_exec('nproc') ? shell_exec('nproc') : 32);

\Workerman\Worker::$pidFile = 'cubex.pid';
\Workerman\Worker::$logFile = 'cubex.log';

\Workerman\Worker::runAll();