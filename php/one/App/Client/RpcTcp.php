<?php
/**
 * Created by PhpStorm.
 * User: admin
 * Date: 2018/12/17
 * Time: 10:15
 * 协程Rpc的Tpc客户端
 */

namespace App\Client;

use One\Facades\Log;
use One\Swoole\Client\Tcp;

class RpcTcp
{
    const RPC_REMOTE_OBJ = '#RpcRemoteObj#';

    private $_need_close = 0;

    /**
     * @var null|Tcp
     */
    protected $_connection = null;

    protected $_connection_key = 'rpc';

    private static $_is_static = 0;

    protected $_remote_class_name = '';

    protected $_token = '';

    public static $_call_id = '';

    protected $_id = '';

    protected $_class = '';

    protected $_args = [];

    public function __construct(...$args)
    {
        $this->_id    = Log::getTraceId();
        $this->_class = $this->_remote_class_name ? $this->_remote_class_name : get_called_class();
        $this->_args  = $args;
        if ($this->_connection === null) {
            // 加载配置文件信息
            $this->_connection = new Tcp($this->_connection_key);
        }
    }

    public function __call($name, $arguments)
    {
        return $this->_callRpc([
            'i' => $this->_id, // 分布式唯一id
            'c' => $this->_class, // 调用class
            'f' => $name, // 调用方法名称
            'a' => $arguments, // 调用方法参数
            't' => $this->_args, // 构造函数参数 __construct
            's' => self::$_is_static, // 是否是静态方法
            'o' => $this->_token, // token 在中间件可获取
        ]);
    }

    private function _callRpc($data)
    {
        self::$_is_static = 0;
        $data             = msgpack_pack($data);
        $data             = $this->_connection->call($data);
        $data             = msgpack_unpack($data);

        if ($data === self::RPC_REMOTE_OBJ) {
            $this->_need_close = 1;
            return $this;
        } else if (is_array($data) && isset($data['err'], $data['msg'])) {
            throw new \Exception($data['msg'], $data['err']);
        } else {
            return $data;
        }
    }

    public function setStaticMethod()
    {
        self::$_is_static = 1;
        return $this;
    }

    public static function __callStatic($name, $arguments)
    {
        self::$_is_static = 1;
        return (new static)->{$name}(...$arguments);
    }

    public function __destruct()
    {
        if ($this->_need_close) {
            $this->_callRpc(['i' => $this->_id]);
        }
    }
}
