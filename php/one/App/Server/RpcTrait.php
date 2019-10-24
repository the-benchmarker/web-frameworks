<?php
/**
 * Created by PhpStorm.
 * User: admin
 * Date: 2018/12/6
 * Time: 17:32
 */

namespace App\Server;

use One\Facades\Log;
use One\Swoole\RpcServer;

trait RpcTrait
{
    private function callRpc($data, $ide = 0, $host = '', $px = '')
    {
        try {
            $arr = msgpack_unpack($data);
            if (isset($arr['c'])) {
                $go_id = Log::setTraceId($arr['i'] . '.' . uuid());
                $str   = msgpack_pack(RpcServer::call($arr));
                Log::flushTraceId($go_id);
            } elseif ($ide === 1) {
                $str = RpcServer::ideHelper($host, $px);
            } else {
                $str = msgpack_pack('params error');
            }
            return $str;
        } catch (\Throwable $e) {
            if (isset($go_id)) {
                Log::flushTraceId($go_id);
            }
            error_report($e);
            return msgpack_pack([
                'err' => $e->getCode(),
                'msg' => $e->getMessage()
            ]);
        }
    }
}
