<?php
/**
 * Created by PhpStorm.
 * User: tanszhe
 * Date: 2018/8/24
 * Time: 下午4:59
 */

namespace App\Server;

use App\Cloud\Server;
use One\Swoole\Server\WsServer;

class AppWsServer extends WsServer
{
// 分布式
//    protected $cloud_server = null;
//
//    public function __construct(\swoole_server $server, array $conf)
//    {
//        parent::__construct($server, $conf);
//        $this->cloud_server = new Server($this);
//    }

    public function onHandShake(\swoole_http_request $request, \swoole_http_response $response)
    {
        return parent::onHandShake($request, $response);
    }

    public function onMessage(\swoole_websocket_server $server, \swoole_websocket_frame $frame)
    {
        $this->wsRouter($server, $frame);
    }

    public function onOpen(\swoole_websocket_server $server, \swoole_http_request $request)
    {
//        $this->session[$request->fd]->get('user');
        return true;
    }

    public function onClose(\swoole_server $server, $fd, $reactor_id)
    {
        parent::onClose($server, $fd, $reactor_id);
        unset($this->session[$fd]);
    }
}