<?php
/**
 * Created by PhpStorm.
 * User: admin
 * Date: 2018/12/6
 * Time: 15:08
 */

namespace App\Server;

use One\Swoole\Listener\Tcp;

class RpcTcpPort extends Tcp
{
    use RpcTrait;

    public function onReceive(\swoole_server $server, $fd, $reactor_id, $data)
    {
        $str = $this->callRpc($data);
        $this->send($fd, $str);
    }
}