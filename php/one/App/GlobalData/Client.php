<?php
/**
 * Created by PhpStorm.
 * User: tanszhe
 * Date: 2018/10/12
 * Time: 下午9:55
 */

namespace App\GlobalData;

use One\Swoole\Client\Tcp;

/**
 * Class Client
 * @package App\GlobalData
 * @mixin Data
 */
class Client
{
    private $client;

    public function __construct()
    {
        $this->client = new Tcp('global_data');
    }

    public function __call($name, $arguments)
    {
        $data = msgpack_pack(['m' => $name, 'args' => $arguments]);
        $res  = $this->client->call($data);
        $ret  = msgpack_unpack($res);
        return $ret;
    }
}