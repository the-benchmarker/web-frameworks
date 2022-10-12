<?php

return [
    'mode' => SWOOLE_BASE ,
    'host' => '0.0.0.0',
    'port' => 3000 ,
    'sockType' => SWOOLE_SOCK_TCP ,
    'additional' => [
        'worker_num'       => swoole_cpu_num() * 2,
    ],

    'ssl' => [
        'ssl_cert_file' => null ,
        'ssl_key_file' => null ,
    ] ,

    'services' => [] ,

    'watcher' => [
        'app',
        'config',
        'database',
        'language',
        'routes',
        'composer.lock',
        '.env',
    ] ,

    'advanceMode' => [
        'request' => DISABLE
    ]
];