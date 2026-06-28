<?php

// =============================================================================
// PRODUCTION CONFIGURATION FOR IMISWOOLE FRAMEWORK
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

    // 主服务器配置 - Production optimizations
    'mainServer'    =>    [
        'namespace' =>  'ImiApp\ApiServer',
        'type'      =>  Imi\Swoole\Server\Type::HTTP,
        'host'      =>  '0.0.0.0',
        'port'      =>  3000,
        'mode'      =>  SWOOLE_BASE,
        'configs'   =>  [
            'worker_num'       => swoole_cpu_num() * 2,
            'enable_coroutine' => false,
            'open_tcp_nodelay' => true,
            'tcp_fastopen'     => true,
            'log_file'         => '/dev/null',
            'log_level'        => SWOOLE_LOG_ERROR,
            'daemonize'        => false,
            'max_request'     => 10000,
        ],
    ],

];
