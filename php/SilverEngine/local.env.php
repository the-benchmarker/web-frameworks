<?php

/*
|--------------------------------------------------------------------------
| # Environment master settings
|--------------------------------------------------------------------------
*/

return [
    'debug'         => true,
    'app_key'       => 'mysupersecurekey',


    'middlewares'   => [

    ],

    'routes' => [
      
    ],

    'databases' => [
        'on' => true,
        'default' => 'local',
        'local'   => [
            'service'       => true,
            'driver'        => 'mysql',
            'hostname'      => 'localhost',
            'username'      => 'root',
            'password'      => '',
            'basename'      => 'databasename',
            'limit_request' => 25,
        ]
    ],

    'mail' => [
        'service' => false,
        'email'   => 'your@email.test',
        'name'    => 'Your Name',
    ],

];
