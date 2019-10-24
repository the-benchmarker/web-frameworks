<?php
/**
 * Created by PhpStorm.
 * User: admin
 * Date: 2018/10/22
 * Time: 15:57
 */

return [
    'server' => [
        'server_type'   => \One\Swoole\OneServer::SWOOLE_SERVER, // 主服务器类型
        'port'          => 9086,
        'action'        => \App\GlobalData\Server::class, // 主服务器事件回调类
        'mode'          => SWOOLE_BASE,
        'sock_type'     => SWOOLE_SOCK_TCP,
        'ip'            => '127.0.0.1',
        'pack_protocol' => \One\Protocol\Frame::class, // tcp 打包 解包协议
        'set'           => [ // set 相关配置
            'worker_num'          => 1,
            'reactor_num'         => 1,
            'open_length_check'   => 1,
            'package_length_func' => '\One\Protocol\Frame::length',
            'package_body_offset' => \One\Protocol\Frame::HEAD_LEN,
        ],
        'save_path'     => _APP_PATH_ . '/RunCache/data.msg'
    ]
];
