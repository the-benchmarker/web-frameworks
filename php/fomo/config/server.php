<?php

return [
    'mode' => SWOOLE_BASE ,
    'host' => '127.0.0.1',
    'port' => 9000 ,
    'sockType' => SWOOLE_SOCK_TCP ,
    'additional' => [
        'worker_num' => cpuCount() * 2 ,
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