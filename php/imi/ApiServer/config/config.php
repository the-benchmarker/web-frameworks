<?php
return [
    'configs'    =>    [
    ],
    'beanScan'    =>    [
        'ImiApp\ApiServer\Controller',
    ],
    'beans'    =>    [
        'HttpDispatcher'    =>    [
            'middlewares'    =>    [
                \Imi\Server\Http\Middleware\RouteMiddleware::class,
            ],
        ],
    ],
    'controller'    =>  [
        'singleton' => true,
    ],
];
