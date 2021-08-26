<?php
return [
    // 项目根命名空间
    'namespace'    =>    'ImiApp',

    // 配置文件
    'configs'    =>    [
        'beans'        =>    __DIR__ . '/beans.php',
    ],

    // 主服务器配置
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
        ],
    ],

];
