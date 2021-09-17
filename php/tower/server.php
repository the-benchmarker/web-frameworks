<?php

require_once __DIR__ . '/vendor/autoload.php';

use Dotenv\Dotenv;
use Tower\Application;
use Tower\Loader;
use Workerman\Connection\TcpConnection;
use Workerman\Worker;

Dotenv::createImmutable(basePath())->load();

$app = include configPath() . "app.php";

Loader::save([
    'app' => configPath() . "app.php" ,
    'database' => configPath() . "database.php" ,
    'elastic' => configPath() . "elastic.php" ,
    'mail' => configPath() . "mail.php" ,
    'redis' => configPath() . "redis.php" ,
    'server' => configPath() . "server.php" ,
]);

$serverConfig = Loader::get('server');
$appConfig = Loader::get('app');

Worker::$pidFile = storagePath() . 'tower.pid';
Worker::$stdoutFile = storagePath() . 'logs/tower.log';
TcpConnection::$defaultMaxPackageSize = $serverConfig['max_package_size'] ?? 10*1024*1024;

$worker = new Worker($serverConfig['listen'], $serverConfig['context']);

$properties = [
    'name',
    'count',
    'user',
    'group',
    'reusePort',
    'transport',
];
foreach ($properties as $property){
    if (isset($serverConfig[$property])){
        $worker->$property = $serverConfig[$property];
    }
}

$boot = new Application();

$worker->onWorkerStart = [$boot , 'onWorkerStart'];

$worker->onMessage = [$boot , 'onMessage'];

Worker::runAll();
