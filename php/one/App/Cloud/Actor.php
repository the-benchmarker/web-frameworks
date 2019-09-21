<?php
/**
 * Created by PhpStorm.
 * User: admin
 * Date: 2019/5/5
 * Time: 16:26
 */

namespace App\Cloud;

use One\ConfigTrait;

class Actor
{
    use ConfigTrait;

    /**
     * @var \One\Swoole\Server
     */
    protected static $server = null;

    /**
     * @var Client
     */
    private static $client = null;

    protected $actor_id = '';

    private function __construct()
    {
        if (!self::$client) {
            self::$client = new Client('');
            self::$client->setActor();
        }

        $str = uniqid('', true);
        $arr = explode('.', $str);
        $str = $arr[0] . base_convert($arr[1], 10, 16);

        $this->actor_id = self::$conf['self_key'] . '.' . self::$server->worker_id . '.' . $str;
    }

    private function __clone()
    {
    }

    private static $actors = [];

    public static function setServer($server)
    {
        self::$server = $server;
    }

    /**
     * @return $this
     */
    public static function init()
    {
        $act                          = new static;
        self::$actors[$act->actor_id] = $act;
        return $act;
    }

    public static function drop($actor_id)
    {
        unset(self::$actors[$actor_id]);
    }

    /**
     * @param $key
     * @return Server
     */
    public function call($actor_id, $method, $args)
    {
        $arr = explode('.', $actor_id);
        if ($arr[0] == self::$conf['self_key'] && $arr[1] == self::$server->worker_id) { // 同机器 同进程
            return self::dispatch($actor_id, $method, $args);
        } else if ($arr[0] == self::$conf['self_key']) { // 同机器 其他进程
            return self::$server->sendMessage([$actor_id, $method, $args], $arr[1]);
        } else { // 其他机器
            return self::$client->setConnect($arr[0])->setStaticMethod()->router($actor_id, $method, $args);
        }
    }

    public static function router($actor_id, $method, $args)
    {
        $arr = explode('.', $actor_id);
        if ($arr[1] == self::$server->worker_id) {
            return self::dispatch($actor_id, $method, $args);
        } else {
            return self::$server->sendMessage([$actor_id, $method, $args], $arr[1]);
        }
    }

    public static function dispatch($actor_id, $method, $args)
    {
        return self::$actors[$actor_id]->$method(...$args);
    }
}

/*
//需要在 onPipeMessage事件函数 调用
Actor::dispatch(...$message);

class user extends Actor
{

}

$user1 = user::init();

$user2 = user::init();

*/