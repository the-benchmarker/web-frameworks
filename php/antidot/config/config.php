<?php

declare(strict_types=1);

use Laminas\ConfigAggregator\ArrayProvider;
use Laminas\ConfigAggregator\ConfigAggregator;
use Laminas\ConfigAggregator\PhpFileProvider;

ini_set('memory_limit', '256M');

$cacheConfig = [
    'config_cache_path' => 'var/cache/config-cache.php',
];

$aggregator = new ConfigAggregator([
    \Antidot\Cli\Container\Config\ConfigProvider::class,
    \Antidot\Framework\Config\ConfigProvider::class,
    \Antidot\Runtime\Config\ConfigProvider::class,
    new PhpFileProvider(realpath(__DIR__).'/services/{{,*.}prod,{,*.}local,{,*.}dev}.php'),
    new ArrayProvider($cacheConfig),
], $cacheConfig['config_cache_path']);

$config = $aggregator->getMergedConfig();
$cliConfig['factories'] = $config['console']['factories'] ?? [];

return array_merge_recursive($config, $cliConfig);
