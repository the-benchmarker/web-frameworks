<?php

declare(strict_types=1);

use Psr\Cache\CacheItemPoolInterface;
use Symfony\Component\Cache\Adapter\ArrayAdapter;

use function DI\create;

return [
    // TODO: It must be replaced by something like Redis.
    CacheItemPoolInterface::class => create(ArrayAdapter::class),
];
