<?php

declare(strict_types=1);

namespace App\Router;

use Nette\StaticClass;
use Nette\Application\Routers\RouteList;

final class RouterFactory
{
    use StaticClass;

    public static function createRouter(): RouteList
    {
        $router = new RouteList();
        $router->addRoute('user[/<id>]', 'User:default');
        $router->addRoute('<presenter>/<action>[/<id>]', 'Homepage:default');
        return $router;
    }
}
