<?php
/**
 * Created by PhpStorm.
 * User: admin
 * Date: 2019/4/10
 * Time: 13:46
 */

namespace App\Cloud;

use One\ConfigTrait;

/**
 * Class Server
 * @package App\Cloud
 */
class Server
{
    use ConfigTrait;

    /**
     * @var \One\Swoole\Server
     */
    private static $server = null;

    /**
     * @var Client
     */
    private static $client = null;

    /**
     * @var \App\GlobalData\Client
     */
    private static $global_data = null;


    public function __construct($server = null)
    {
        if ($server) {
            self::$client      = new Client('');
            self::$server      = $server;
            self::$global_data = new \App\GlobalData\Client();
        }
    }

    public function getServer()
    {
        return self::$server;
    }

    /**
     * tcp 向id发送消息 分布式
     * @param $id
     * @param $msg
     */
    public function sendById($id, $msg)
    {
        $fds = self::$global_data->getFdById($id);
        foreach ($fds as $fd) {
            $i = strpos($fd, '@');
            if ($i !== false) {
                $name = substr($fd, 0, $i);
                if ($this->getServerName() == $name) {
                    $this->send(substr($fd, $i + 1), $msg);
                } else {
                    $this->remote($name)->send(substr($fd, $i + 1), $msg);
                }
            } else {
                $this->send($fd, $msg);
            }
        }
    }

    public function send($fd, $msg)
    {
        $this->getServer()->send($fd, $msg);
    }

    public function push($fd, $msg)
    {
        $this->getServer()->push($fd, $msg);
    }


    /**
     * websocket 向id发送消息
     * @param $id
     * @param $msg
     */
    public function pushById($id, $msg)
    {
        $fds = self::$global_data->getFdById($id);
        foreach ($fds as $fd) {
            $i = strpos($fd, '@');
            if ($i !== false) {
                $name = substr($fd, 0, $i);
                if ($this->getServerName() == $name) {
                    $this->push(substr($fd, $i + 1), $msg);
                } else {
                    $this->remote($name)->push(substr($fd, $i + 1), $msg);
                }
            } else {
                $this->push($fd, $msg);
            }
        }
    }

    /**
     * 获取本机器名称
     * @return mixed
     */
    public function getServerName()
    {
        return self::$conf['self_key'];
    }

    /**
     * 获取完整fd
     * @param $fd
     * @return string
     */
    public function getFullFd($fd)
    {
        return $this->getServerName() . '@' . $fd;
    }


    /**
     * @param $key
     * @return Server
     */
    public function remote($key)
    {
        return self::$client->setConnect($key);
    }
}