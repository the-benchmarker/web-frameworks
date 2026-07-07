<?php

// =============================================================================
// PRODUCTION CONFIGURATION FOR IMWORKERMAN FRAMEWORK
// =============================================================================

error_reporting(E_ALL);
ini_set('display_errors', '0');
ini_set('display_startup_errors', '0');
ini_set('log_errors', '1');
ini_set('log_errors_max_len', '1024');
ini_set('ignore_repeated_errors', '1');
ini_set('ignore_repeated_source', '1');
ini_set('html_errors', '0');

define('DEBUG_MODE', false);

// Security settings
ini_set('expose_php', '0');
ini_set('session.cookie_httponly', '1');
ini_set('session.cookie_secure', '1');

// Performance settings
ini_set('memory_limit', '256M');

return [
    // 项目根命名空间
    'namespace'    =>    'ImiApp',

    // 配置文件
    'configs'    =>    [
        'beans'        =>    __DIR__ . '/beans.php',
    ],

    // Workerman 服务器配置 - Production optimizations
    'workermanServer' => [
        // 服务器名，http 也可以改成 abc 等等，完全自定义
        'http' => [
            // 指定服务器命名空间
            'namespace' => 'ImiApp\ApiServer',
            // 服务器类型
            'type'      => Imi\Workerman\Server\Type::HTTP, // HTTP、WEBSOCKET、TCP、UDP
            'host'      => '0.0.0.0',
            'port'      => 3000,
            // socket的上下文选项，参考：http://doc3.workerman.net/315128
            'context'   => [],
            'configs'   => [
                // 支持设置 Workerman 参数
                'count' => shell_exec('nproc') ?: 32,
                'name' => 'Imi Workerman Benchmark Server',
            ],
        ],
    ],

];
