<?php

declare(strict_types=1);
/**
 * This file is part of Simps.
 *
 * @link     https://simps.io
 * @document https://doc.simps.io
 * @license  https://github.com/simple-swoole/simps/blob/master/LICENSE
 */

namespace App\Controller;

use Simps\Server\Protocol\Http\SimpleResponse;

class IndexController
{
    public static function onReceive($server, $fd, $from_id, $data)
    {
        $first_line = \strstr($data, "\r\n", true);
        $tmp = \explode(' ', $first_line, 3);
        $path = isset($tmp[1]) ? $tmp[1] : '/';
        if (0 === \strpos($path, '/user/id/') && isset($path[9])) {
            $response = SimpleResponse::build(\substr($path, 9));
        } else {
            $response = SimpleResponse::build("");
        }
        $server->send($fd, $response);
    }
}