<?php
/**
 * Created by PhpStorm.
 * User: tanszhe
 * Date: 2018/8/24
 * Time: 下午4:59
 * 带路由Tcp
 */

namespace App\Server;

use One\Protocol\TcpRouterData;
use One\Swoole\Listener\Tcp;

class AppTcpPort extends Tcp
{

    /**
     * @param \swoole_server $server
     * @param $fd
     * @param $reactor_id
     * @param TcpRouterData $data
     */
    public function onReceive(\swoole_server $server, $fd, $reactor_id, $data)
    {
        $this->tcpRouter($server,$fd,$reactor_id,$data);
    }
}