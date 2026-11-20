<?php

declare(strict_types=1);
/**
 * This file is part of Hyperf.
 *
 * @link     https://www.hyperf.io
 * @document https://doc.hyperf.io
 * @contact  group@hyperf.io
 * @license  https://github.com/hyperf/hyperf/blob/master/LICENSE
 */

namespace App\Controller;

class IndexController
{
    public function index()
    {
        return '';
    }

    public function get(int $id)
    {
        return (string) $id;
    }

    public function create()
    {
        return '';
    }
}
