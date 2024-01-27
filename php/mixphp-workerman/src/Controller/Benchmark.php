<?php

namespace App\Controller;

use Mix\Vega\Context;

class Benchmark
{
    /**
     * @param Context $ctx
     */
    public function index(Context $ctx)
    {
        $ctx->string(200, '');
    }

    /**
     * @param Context $ctx
     */
    public function get(Context $ctx)
    {
        $ctx->string(200, $ctx->request->param['id'] ?? '');
    }

    /**
     * @param Context $ctx
     */
    public function create(Context $ctx)
    {
        $ctx->string(200, '');
    }

}
