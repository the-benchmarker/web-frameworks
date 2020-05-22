<?php

define('_APP_PATH_', __DIR__);

define('_APP_PATH_VIEW_', __DIR__ . '/View');

function uuid()
{
    return '1';
}

//define('_DEBUG_',true);

require __DIR__ . '/../vendor/autoload.php';
require __DIR__ . '/../vendor/lizhichao/one/src/run.php';
require __DIR__ . '/config.php';


\One\Swoole\OneServer::parseArgv();

//\Swoole\Runtime::enableCoroutine();

\One\Swoole\OneServer::runAll();
