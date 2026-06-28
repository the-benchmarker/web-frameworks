<?php

declare(strict_types=1);

use Psr\Container\ContainerInterface;
use Mezzio\Application;
use Mezzio\MiddlewareFactory;

/**
 * Mezzio Framework Routes
 * 
 * Benchmark server routes following PHP best practices.
 */
return function (Application $app, MiddlewareFactory $factory, ContainerInterface $container): void {
    // Root endpoint
    $app->route('/', \App\Handler\Home::class, ['GET']);
    
    // Health check endpoint
    $app->route('/health', \App\Handler\Health::class, ['GET']);
    
    // User endpoints
    $app->route('/user/{id}', \App\Handler\UserId::class, ['GET']);
    $app->route('/user', \App\Handler\User::class, ['POST']);
};
