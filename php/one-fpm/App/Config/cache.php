<?php
return [
    'drive' => 'file', // [file | redis]

    'file' => [
        'path' => _APP_PATH_ . '/RunCache/cache',
        'prefix' => 'one_'
    ],

    'redis' => [
        'default' => [
            'max_connect_count' => 10,
            'host' => '127.0.0.1',
            'port' => 6379,
            'prefix' => 'one_',
//            'auth' => '123456'
        ],
        'default_cluster' => [
            'max_connect_count' => 10,
            'args' => [
                null,
                ['192.168.1.10:7000','192.168.1.10:7001'],
                1.5,
                1.5,
                false,
                'password'
            ],
            'is_cluster' => true,
            'prefix' => 'one_',
//            'auth' => '123456'
        ]
    ]
];
