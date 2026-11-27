<?php

namespace App;

class Router extends \MacropaySolutions\Framework\Routing\Router
{
    public function registerRoutes(): void
    {
        $this->group([
            'namespace' => 'App\Http\Controllers',
        ], static function (\App\Router $router): void {
            require $router->app->basePath('routes/web.php');
        });
    }
}
