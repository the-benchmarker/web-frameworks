<?php
declare(strict_types=1);

use Siler\Swoole;
use Siler\Route;
use Siler\Functional as λ;

chdir(dirname(dirname(__DIR__)));
require __DIR__ . '/../vendor/autoload.php';

$handler = function () {
    Route\get('/', function () {
        Swoole\emit('');
    });
    Route\get('/user/{id}', function ($params) {
        Swoole\emit($params['id']);
    });
    Route\post('/user', function () {
        Swoole\emit('');
    });

    // None of the above short-circuited the response with Swoole\emit().
    Swoole\emit('Not found', 404);
};

Swoole\http($handler, 3000)->start();