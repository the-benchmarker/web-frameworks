<?php

namespace App;

use App\Controller\Benchmark;
use Mix\Vega\Engine;

class Vega
{

    /**
     * @return Engine
     */
    public static function new(): Engine
    {
        $vega = new Engine();
        $vega->mode(Engine::FAST_MODE);
        $benchmark = new Benchmark();
        $vega->handle('/', [$benchmark, 'index'])->methods('GET');
        $vega->handle('/user', [$benchmark, 'create'])->methods('POST');
        $vega->handle('/user/{id:\d+}', [$benchmark, 'get'])->methods('GET');
        return $vega;
    }

}
