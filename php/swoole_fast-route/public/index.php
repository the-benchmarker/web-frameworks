<?php

declare(strict_types=1);

require_once __DIR__ . '/../vendor/autoload.php';

use Swoole\Http\Server;
use Swoole\Http\Request;
use Swoole\Http\Response;

function index(array $vars)
{
    return '';
}

function get_user(array $vars)
{
    return $vars['id'];
}

function post_user(array $vars)
{
    return '';
}

$dispatcher = FastRoute\cachedDispatcher(function (FastRoute\RouteCollector $r) {
    $r->addRoute('GET', '/', 'index');
    $r->addRoute('GET', '/user/{id:\d+}', 'get_user');
    $r->addRoute('POST', '/user', 'post_user');
}, [
    'cacheFile' => __DIR__ . '/route.cache',
]);

$host = '0.0.0.0';
$port = 3000;

$server = new Server($host, $port, SWOOLE_BASE);
$server->set(['worker_num' => swoole_cpu_num() * 2]);

$server->on('request', function (Request $request, Response $response) use ($dispatcher) {
    $request_method = $request->server['request_method'];
    $request_uri = $request->server['request_uri'];
    list($code, $handler, $vars) = $dispatcher->dispatch($request_method, $request_uri);
    $result = null;
    switch ($code) {
        case FastRoute\Dispatcher::FOUND:
            $result = $handler($vars);
            $response->header('Content-Type', 'text/plain; charset=utf-8');
            break;
        case FastRoute\Dispatcher::NOT_FOUND:
            $response->status(404);
            break;
        case FastRoute\Dispatcher::METHOD_NOT_ALLOWED:
            $response->status(405);
            break;
    }
    $response->end($result);
});

$server->start();