<?php

declare(strict_types=1);

use Sunrise\Http\Factory\ResponseFactory;
use Sunrise\Http\Factory\ServerRequestFactory;
use Sunrise\Http\Router\Exception\MethodNotAllowedException;
use Sunrise\Http\Router\Exception\RouteNotFoundException;
use Sunrise\Http\Router\Loader\DescriptorLoader;
use Sunrise\Http\Router\Router;
use Symfony\Component\Cache\Adapter\ApcuAdapter;
use Symfony\Component\Cache\Psr16Cache as Cache;

use function Sunrise\Http\Router\emit;

require_once __DIR__ . '/../vendor/autoload.php';

$loader = new DescriptorLoader();
$loader->setCache(new Cache(new ApcuAdapter()));
$loader->attach(App\Controller\HomeController::class);
$loader->attach(App\Controller\User\UserCreateController::class);
$loader->attach(App\Controller\User\UserReadController::class);

$router = new Router();
$router->load($loader);

$request = ServerRequestFactory::fromGlobals();

try {
    emit($router->handle($request));
} catch (MethodNotAllowedException $e) {
    emit((new ResponseFactory())->createResponse(405));
} catch (RouteNotFoundException $e) {
    emit((new ResponseFactory())->createResponse(404));
} catch (Throwable $e) {
    emit((new ResponseFactory())->createResponse(500));
}
