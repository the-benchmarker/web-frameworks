<?php

declare(strict_types=1);

use Doctrine\Common\Annotations\AnnotationRegistry;
use Sunrise\Http\Message\ResponseFactory;
use Sunrise\Http\Router\Exception\MethodNotAllowedException;
use Sunrise\Http\Router\Exception\RouteNotFoundException;
use Sunrise\Http\Router\Loader\AnnotationDirectoryLoader;
use Sunrise\Http\Router\Router;
use Sunrise\Http\ServerRequest\ServerRequestFactory;

use function Sunrise\Http\Router\emit;

require __DIR__ . '/../vendor/autoload.php';

AnnotationRegistry::registerLoader('class_exists');

$loader = new AnnotationDirectoryLoader();
$loader->attach(__DIR__ . '/../src/Controller');

// [!] Use cache in production
// @see https://www.php-fig.org/psr/psr-16/
// $loader->setCache(...);

$router = new Router();
$router->load($loader);

try {
	emit($router->handle(ServerRequestFactory::fromGlobals()));
} catch (MethodNotAllowedException $e) {
	emit((new ResponseFactory)->createResponse(405));
} catch (RouteNotFoundException $e) {
	emit((new ResponseFactory)->createResponse(404));
}
