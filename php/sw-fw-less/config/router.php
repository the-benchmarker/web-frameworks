<?php

use App\services\TestService;

return [
    'single' => [
        ['GET', '/', [TestService::class, 'index']],
        ['GET', '/user/{id:\d+}', [TestService::class, 'get']],
        ['POST', '/user', [TestService::class, 'create']],
    ],
    'group' => [],
];
