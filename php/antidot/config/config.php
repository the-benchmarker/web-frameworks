<?php

declare(strict_types=1);

use Antidot\DevTools\Container\Config\ConfigProvider as DevToolsConfigProvider;
use Antidot\SymfonyConfigTranslator\Container\Config\ConfigAggregator;
use Antidot\Yaml\YamlConfigProvider;
use Laminas\ConfigAggregator\ArrayProvider;
use Laminas\ConfigAggregator\PhpFileProvider;

// To enable or disable caching, set the `ConfigAggregator::ENABLE_CACHE` boolean in
// `config/autoload/local.php`.
$cacheConfig = [
    'config_cache_path' => 'var/cache/config-cache.php',
];

$aggregator = new ConfigAggregator([
    \Antidot\Fast\Router\Container\Config\ConfigProvider::class,
    \Antidot\Container\Config\ConfigProvider::class,
    \Antidot\React\Container\Config\ConfigProvider::class,
    new PhpFileProvider(realpath(__DIR__).'/services/{{,*.}prod,{,*.}local,{,*.}dev}.php'),
    new YamlConfigProvider(realpath(__DIR__).'/services/{{,*.}prod,{,*.}local,{,*.}dev}.yaml'),
    new ArrayProvider($cacheConfig),
], $cacheConfig['config_cache_path']);

return $aggregator->getMergedConfig();
