<?php

declare(strict_types=1);

use Psr\Cache\CacheItemPoolInterface;
use Psr\SimpleCache\CacheInterface;
use Symfony\Component\Cache\Adapter\ArrayAdapter;
use Symfony\Component\Cache\Psr16Cache;

use function DI\create;
use function DI\get;

return [
    CacheItemPoolInterface::class => create(ArrayAdapter::class),

    CacheInterface::class => create(Psr16Cache::class)
        ->constructor(
            get(CacheItemPoolInterface::class),
        ),
];
