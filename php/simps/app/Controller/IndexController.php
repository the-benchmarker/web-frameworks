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

class IndexController
{
    public function index($request, $response)
    {
        $response->end("");
    }

    public function get($request, $response, $data)
    {
        $response->end((string)$data["id"] ?? "");
    }

    public function create($request, $response)
    {
        $response->end("");
    }
}
