<?php

return [
    'switch' => envInt('POOL_SWITCH', 1),
    'objects' => [
        \SwFwLess\middlewares\Route::class => [
            'pool_size' => envInt('ROUTER_POOL_SIZE', 5),
        ],
    ],
];
