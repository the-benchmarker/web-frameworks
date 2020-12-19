<?php
\One\Http\Router::setConfig(['path' => _APP_PATH_ . '/Config/router.php']);


\One\Swoole\OneServer::setConfig(config('protocol', true));


\One\Http\Router::loadRouter();
