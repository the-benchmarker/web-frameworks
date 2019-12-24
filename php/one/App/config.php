<?php
\One\Http\Router::setConfig(['path' => _APP_PATH_ . '/Config/router.php']);

// 分布式配置
\One\Swoole\OneServer::setConfig(config('protocol', true));

// 解析路由
\One\Http\Router::loadRouter();
