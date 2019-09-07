<?php
return [
    // 项目根命名空间
    'namespace'    =>    'ImiApp',

    // 配置文件
    'configs'    =>    [
        'beans'        =>    __DIR__ . '/beans.php',
    ],

    // 扫描目录
    'beanScan'    =>    [],

    // 组件命名空间
    'components'    =>  [],

    // 主服务器配置
    'mainServer'    =>    [
        'namespace' =>  'ImiApp\ApiServer',
        'type'      =>  Imi\Server\Type::HTTP,
        'host'      =>  '127.0.0.1',
        'port'      =>  3000,
        'configs'   =>  [],
    ],
];
