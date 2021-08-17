<?php

use Swoft\Http\Server\HttpServer;

return [
    'logger'         => [
        'flushRequest' => false,
        'enable'       => false,
        'json'         => false,
    ],
    'httpServer'     => [
        'class'   => HttpServer::class,
        'port'    => 3000,
        'on'      => [],
        /* @see HttpServer::$setting */
        'setting' => [
            'worker_num'       => swoole_cpu_num() * 2,
            'enable_coroutine' => false,
            'log_file' => '/dev/null',
            'log_level' => SWOOLE_LOG_ERROR,
            // 'log_file'   => alias('@runtime/swoole.log'),
        ]
    ],
    'httpDispatcher' => [
        // Add global http middleware
        'middlewares' => [
            // Allow use @View tag
            // \Swoft\View\Middleware\ViewMiddleware::class,
        ],
    ],
];
