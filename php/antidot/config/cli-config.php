<?php

declare(strict_types=1);

use Antidot\SymfonyConfigTranslator\Container\Config\ConfigAggregator;
use Laminas\ConfigAggregator\ArrayProvider;

$config = require __DIR__ . '/config.php';
$cliConfig['services'] = $config['console']['services'] ?? [];
$cliConfig['factories'] = $config['console']['factories'] ?? [];
$cacheConfig = [
    'cli_config_cache_path' => 'var/cache/cli-config-cache.php',
];

return (new ConfigAggregator(
    [
        new ArrayProvider($config),
        new ArrayProvider($cliConfig),
        new ArrayProvider($cacheConfig),
    ], $cacheConfig['cli_config_cache_path']
))->getMergedConfig();
