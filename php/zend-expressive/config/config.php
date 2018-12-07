<?php

declare(strict_types=1);

use Zend\ConfigAggregator\ArrayProvider;
use Zend\ConfigAggregator\ConfigAggregator;
use Zend\ConfigAggregator\PhpFileProvider;

// To enable or disable caching, set the `ConfigAggregator::ENABLE_CACHE` boolean in
// `config/autoload/local.php`.
$cacheConfig = [
    'config_cache_path' => 'data/config-cache.php',
];

$aggregator = new ConfigAggregator([
    \Zend\Expressive\Router\FastRouteRouter\ConfigProvider::class,
    \Zend\HttpHandlerRunner\ConfigProvider::class,
    // Include cache configuration
    new ArrayProvider($cacheConfig),

    \Zend\Expressive\Helper\ConfigProvider::class,
    \Zend\Expressive\ConfigProvider::class,
    \Zend\Expressive\Router\ConfigProvider::class,

    // Swoole config to overwrite some services (if installed)
    class_exists(\Zend\Expressive\Swoole\ConfigProvider::class)
        ? \Zend\Expressive\Swoole\ConfigProvider::class
        : function(){ return[]; },
    // Load application config in a pre-defined order in such a way that local settings
    // overwrite global settings. (Loaded as first to last):
    //   - `global.php`
    //   - `*.global.php`
    //   - `local.php`
    //   - `*.local.php`
    new PhpFileProvider(realpath(__DIR__) . '/autoload/{{,*.}global,{,*.}local}.php'),

    // Load development config if it exists
    new PhpFileProvider(realpath(__DIR__) . '/development.config.php'),
], $cacheConfig['config_cache_path']);

return $aggregator->getMergedConfig();
