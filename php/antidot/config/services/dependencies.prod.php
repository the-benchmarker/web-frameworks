<?php

declare(strict_types=1);

use App\Handler\Home;
use App\Handler\User;
use App\Handler\UserId;
use Fidry\CpuCoreCounter\CpuCoreCounter;
use Fidry\CpuCoreCounter\Finder\DummyCpuCoreFinder;
use Fidry\CpuCoreCounter\Finder\FinderRegistry;

$cpuCounter = new CpuCoreCounter([
    ...FinderRegistry::getDefaultLogicalFinders(),
    new DummyCpuCoreFinder(1),  // Fallback value
]);

return [
    'services' => [
        Home::class => Home::class,
        User::class => User::class,
        UserId::class => UserId::class,
    ],
    'debug' => false,
    'config_cache_enabled' => true,
    'server' => [
        'host' => "0.0.0.0",
        'port' => 3000,
        'max_concurrency' => 512,
        'buffer_size' => 2048,
        'workers' => $cpuCounter->getCount()
    ]
];
