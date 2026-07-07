<?php

declare(strict_types=1);

use Antidot\Framework\Application;
use Psr\Container\ContainerInterface;

/**
 * Antidot Framework Routes
 * 
 * Benchmark server routes following PHP best practices.
 */
return static function (Application $app, ContainerInterface $container): void {
    // Root endpoint
    $app->get('/', [\App\Handler\Home::class], 'home');
    
    // Health check endpoint
    $app->get('/health', [\App\Handler\Health::class], 'health_check');
    
    // User endpoints
    $app->get('/user/{id}', [\App\Handler\UserId::class], 'user_detail');
    $app->post('/user', [\App\Handler\User::class], 'user_list');
};
