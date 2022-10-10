<?php

return [
    'mysql' => [
        'driver'            => 'mysql',
        'host'              => env('DB_HOST' , '127.0.0.1'),
        'port'              => env('DB_PORT' , '3306'),
        'database'          => env('DB_DATABASE' , 'forge'),
        'username'          => env('DB_USERNAME' , 'root'),
        'password'          => env('DB_PASSWORD' , 'root'),
        'unix_socket'       => '',
        'charset'           => 'utf8',
        'collation'         => 'utf8_unicode_ci',
        'prefix'            => '',
        'prefix_indexes'    => true,
        'strict'            => true,
        'engine'            => null,
    ]
];