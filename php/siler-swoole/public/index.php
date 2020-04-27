<?php declare(strict_types=1);

use function Siler\Route\{get, post};
use function Siler\Swoole\{emit, http};

require_once __DIR__ . '/../vendor/autoload.php';

$handler = function () {
    get('/', function () {
        emit('');
    });

    get('/user/{id}', function ($params) {
        emit($params['id']);
    });

    post('/user', function () {
        emit('');
    });
};

$server = http($handler, 3000);
$server->set([
    'enable_coroutine' => false,
    'worker_num' => (int) shell_exec('nproc') ?? 32,
    'only_simple_http' => true,
]);
$server->start();