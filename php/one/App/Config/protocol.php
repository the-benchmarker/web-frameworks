<?php

return [
    'server' => [
        'server_type' => \One\Swoole\OneServer::SWOOLE_HTTP_SERVER,
        'port'        => 3000,
        'action'      => \App\Server\AppHttpServer::class,
        'mode'        => SWOOLE_BASE,
        'sock_type'   => SWOOLE_SOCK_TCP,
        'ip'          => '0.0.0.0',
        'set'         => [
            'worker_num'       => swoole_cpu_num(),
        ],
    ]
];
