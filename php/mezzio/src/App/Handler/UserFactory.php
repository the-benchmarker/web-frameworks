<?php

declare(strict_types=1);

namespace App\Handler;

use Psr\Container\ContainerInterface;

class UserFactory
{
    public function __invoke(ContainerInterface $container) : User
    {
        return new User();
    }
}
