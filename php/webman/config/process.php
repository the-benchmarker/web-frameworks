<?php

use support\Log;
use support\Request;
use app\process\Http;
global $argv;

return [
     // webman process configuration
    'webman' => [ 
        'handler'   => Http::class, // Process handler class
        'listen'    => 'http://0.0.0.0:3000', // Listen address
        'count'     => cpu_count(), // Process count, 4x CPU by default
        'reusePort' => true, // Enable reusePort, distributes connections across worker processes
        'name'      => 'webman',
        'user'      => '', // Process user, should use low-privilege user
        'group'     => '', // Process group, should use low-privilege group
        'eventLoop' => '', // Event loop class, uses server.event_loop when empty
        'context'   => [], // Listen context, e.g. SSL
        'constructor' => [ // Constructor parameters for process handler, here Http class
            'requestClass' => Request::class, // Custom request class
            'logger' => Log::channel('default'), // Logger instance
            'appPath' => app_path(), // App directory path
            'publicPath' => public_path() // Public directory path
        ]
    ]
];
