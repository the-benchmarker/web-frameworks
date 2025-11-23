<?php

declare(strict_types=1);

namespace App\Handler;

use Psr\Container\ContainerInterface;

class HomeFactory
{
    public function __invoke(ContainerInterface $container) : Home
    {
        return new Home();
    }
}
