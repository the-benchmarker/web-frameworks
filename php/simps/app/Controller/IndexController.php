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

use Simps\Server\Protocol\HTTP\SimpleResponse;

class IndexController
{
    public function index($server, $fd)
    {
        $server->send($fd, SimpleResponse::build(''));
    }

    public function get($server, $fd, $data)
    {
        $server->send($fd, SimpleResponse::build((string)$data['id'] ?? ''));
    }

    public function create($server, $fd)
    {
        $server->send($fd, SimpleResponse::build(''));
    }
}
