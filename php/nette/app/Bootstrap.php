<?php

declare(strict_types=1);

namespace App;

use Nette\Configurator;

class Bootstrap
{
    public static function boot(): ?Configurator
    {
        $configurator = new Configurator();
        $configurator->setTempDirectory(__DIR__ . '/../temp');
        $configurator->setDebugMode(false);

        $configurator->addConfig(__DIR__ . '/config/common.neon');
        return $configurator;
    }
}
