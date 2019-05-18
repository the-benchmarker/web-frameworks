<?php

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
    $r->addRoute('GET', '/user/[{id}]', 'get_user');
    $r->addRoute('POST', '/user', 'post_user');
}, [
    'cacheFile' => __DIR__ . '/route.cache',
]);

function handleRequest($dispatcher, string $request_method, string $request_uri)
{
    list($code, $handler, $vars) = $dispatcher->dispatch($request_method, $request_uri);
    switch ($code) {
        case FastRoute\Dispatcher::FOUND:
            $result = call_user_func($handler, $vars);
            break;
        case FastRoute\Dispatcher::NOT_FOUND:
        case FastRoute\Dispatcher::METHOD_NOT_ALLOWED:
            $result = $code;
            break;
    }
    return $result;
}

$server = new Server('0.0.0.0', 3000, SWOOLE_BASE);
$server->set(['worker_num' => swoole_cpu_num() * 2]);

$server->on('request', function (Request $request, Response $response) use ($dispatcher) {
    $request_method = $request->server['request_method'];
    $request_uri = $request->server['request_uri'];
    $result = handleRequest($dispatcher, $request_method, $request_uri);
    $response->header('Content-Type', 'text/plain; charset=utf-8');
    $response->end($result);
});

$server->start();