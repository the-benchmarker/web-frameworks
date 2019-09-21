<?php
//\One\Database\Mysql\Connect::setConfig(config('mysql', true));
\One\Log::setConfig(config('log', true));
\One\Http\Router::setConfig(['path' => _APP_PATH_ . '/Config/router.php']);
\One\Cache\File::setConfig(config('cache.file', true));
\One\Cache\Redis::setConfig(config('cache.redis', true));
\One\Crypt\Openssl::setConfig(config('crypt', true));
\One\Exceptions\Handler::setConfig(config('exception', true));

// 分布式配置
\App\Cloud\Server::setConfig(config('cloud', true));
\One\Swoole\OneServer::setConfig(config(isset($argv[1]) ? $argv[1] : 'protocol', true));
\One\Swoole\Client\Tcp::setConfig(config('client', true));

// 加载rpc配置
require _APP_PATH_ . '/Config/rpc.php';

// 解析路由
\One\Http\Router::loadRouter();




