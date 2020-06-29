<?php

declare(strict_types=1);

use Doctrine\Common\Annotations\AnnotationRegistry;
use Sunrise\Http\Router\Loader\AnnotationDirectoryLoader;
use Sunrise\Http\Router\Router;
use Sunrise\Http\ServerRequest\ServerRequestFactory;

use function Sunrise\Http\Router\emit;

require __DIR__ . '/../vendor/autoload.php';

AnnotationRegistry::registerLoader('class_exists');

$loader = new AnnotationDirectoryLoader();
$loader->attach(__DIR__ . '/../src/Controller');

$router = new Router();
$router->load($loader);

emit($router->handle(ServerRequestFactory::fromGlobals()));
