<?php

require_once __DIR__ . '/../vendor/autoload.php';

/*
|--------------------------------------------------------------------------
| Create The Application
|--------------------------------------------------------------------------
|
| Here we will load the environment and create the application instance
| that serves as the central piece of this framework. We'll use this
| application as an "IoC" container and router for this framework.
|
| See also \App\Application::$availableBindings and \App\Application::aliases and Application::abstractAliases
| Uncomment from there and remove from composer.json autoload exclude-from-classmap the extra modules that you need.
|
*/

$app = new App\Application(\dirname(__DIR__));

/**
|--------------------------------------------------------------------------
| Register Container Bindings
|--------------------------------------------------------------------------
|
| Now we will register a few bindings in the service container. We will
| register the exception handler and the console kernel. You may add
| your own bindings here if you like or you can make another file.
|
| Moved to:
* @see \App\Application::bindings
*/

/**
|--------------------------------------------------------------------------
| Register Config Files
|--------------------------------------------------------------------------
|
| Now we will register the "app" configuration file. If the file exists in
| your configuration directory it will be loaded; otherwise, we'll load
| the default version. You may register other files below as needed.
|
| Use run config:cache on production on each deploy to speed up this configuration
*/

if (!$app->configurationIsCached()) {
    $app->configure('app');
    $app->configure('auth');
    $app->configure('broadcasting');
    $app->configure('cache');
    $app->configure('crufd_wizard');
    $app->configure('database');
    $app->configure('filesystems');
    $app->configure('logging');
    $app->configure('queue');
    $app->configure('services');
    $app->configure('session');
    $app->configure('view');
//    $app->configure('mail');
}

/**
|--------------------------------------------------------------------------
| Register Middleware
|--------------------------------------------------------------------------
|
| Next, we will register the middleware with the application. These can
| be global middleware that run before and after each request into a
| route or middleware that'll be assigned to some specific routes.
* @see \App\Application::$middleware
* @see \App\Application::$foundRouteMiddleware
* @see \App\Application::$routeMiddleware
*/

/**
|--------------------------------------------------------------------------
| Register Service Providers
|--------------------------------------------------------------------------
|
| Here we will register all the application's service providers which
| are used to bind services into the container. Service providers are
| totally optional, so you are not required to uncomment this line.
|
| Note that if you want to speed up the boot process, you can move the provider register content into:
* @see \App\Application::$bindings
| Pay attention to $this->app usage in the callbacks. It should become $app as
| the context in which the callback is defined changes.
|
| The register call below is still needed if you have logic in boot method
*/

// $app->register(\App\Providers\AppServiceProvider::class);
/**
 * @see \App\Application::registerAuthBindings for \App\Providers\AuthServiceProvider::class
 * @see \App\Application::registerEventBindings for \App\Providers\EventServiceProvider::class
 */

/**
|--------------------------------------------------------------------------
| Load The Application Routes
|--------------------------------------------------------------------------
|
| Next we will include the routes file so that they can all be added to
| the application. This will provide all the URLs the application
| can respond to, as well as the controllers that may handle them.
|
| Use run route:cache on production on each deploy to speed up route registration
*/

if (!$app->routesAreCached()) {
    $app->router->registerRoutes();
}

return $app->boot(
    // \App\Providers\AppServiceProvider::class,
);
