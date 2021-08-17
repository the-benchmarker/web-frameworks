<?php
/**
 * Created by PhpStorm.
 * User: tanszhe
 * Date: 2018/8/24
 * Time: 下午5:23
 * http,websocket,tcp 服务器配置
 */

return [
    'server' => [
        'server_type' => \One\Swoole\OneServer::SWOOLE_HTTP_SERVER,
        'port'        => 3000,
        'action'      => \App\Server\AppHttpServer::class,
        'mode'        => SWOOLE_BASE,
        'sock_type'   => SWOOLE_SOCK_TCP,
        'ip'          => '0.0.0.0',
        'set'         => [
            'worker_num'       => swoole_cpu_num() * 2,
            'enable_coroutine' => false,
            'log_file' => '/dev/null',
            'log_level' => SWOOLE_LOG_ERROR,
        ],
    ]
];
