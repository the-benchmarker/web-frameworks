<?php
/**
 * Created by PhpStorm.
 * User: admin
 * Date: 2018/12/6
 * Time: 15:08
 */

namespace App\Server;

use One\Swoole\Server\TcpServer;

class RpcTcpServer extends TcpServer
{
    use RpcTrait;

    public function onReceive(\swoole_server $server, $fd, $reactor_id, $data)
    {
        $str = $this->callRpc($data);
        $this->server->send($fd, $str);
    }
}