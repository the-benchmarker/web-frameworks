<?php
return [
    'namespace'    =>    'ImiApp',
    'configs'    =>    [
        'beans'        =>    __DIR__ . '/beans.php',
    ],
    'beanScan'    =>    [],
    'components'    =>  [],
    'mainServer'    =>    [
        'namespace' =>  'ImiApp\ApiServer',
        'type'      =>  Imi\Server\Type::HTTP,
        'host'      =>  '0.0.0.0',
        'port'      =>  3000,
        'mode'      =>  SWOOLE_BASE,
        'configs'   =>  [
            'worker_num'        => swoole_cpu_num(),
            'open_tcp_nodelay'  => true,
            'tcp_fastopen'      => true,
        ],
    ],

    'imi'   =>  [
        'beanScan'  =>  [
            'Imi\Config',
            'Imi\Bean',
            'Imi\Aop',
            'Imi\Annotation',
            'Imi\Server',
            'Imi\Log',
            'Imi\Listener',
            'Imi\Tool',
            'Imi\Process',
        ],
    ],
];
