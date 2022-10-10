<?php

return [
    [
        'host' => env('ELASTICSEARCH_HOST' , '127.0.0.1'),
        'port' => env('ELASTICSEARCH_PORT' , 9200),
        'user' => env('ELASTICSEARCH_USERNAME'),
        'pass' => env('ELASTICSEARCH_PASSWORD')
    ]
];