<?php

declare(strict_types=1);

namespace App\Handler;

use Psr\Container\ContainerInterface;

class UserIdFactory
{
    public function __invoke(ContainerInterface $container) : UserId
    {
        return new UserId();
    }
}
