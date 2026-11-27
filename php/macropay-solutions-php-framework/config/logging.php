<?php

// the benchmark runs with application logging disabled
return [
    'default' => 'null',

    'channels' => [
        'null' => [
            'driver' => 'monolog',
            'handler' => \Monolog\Handler\NullHandler::class,
        ],
    ],
];
