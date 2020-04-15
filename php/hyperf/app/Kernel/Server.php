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
namespace App\Kernel;

use Hyperf\Contract\OnRequestInterface;
use Swoole\Http\Request as SwooleRequest;
use Swoole\Http\Response as SwooleResponse;

class Server implements OnRequestInterface
{
    public function onRequest(SwooleRequest $request, SwooleResponse $response): void
    {
        // TODO: Implement onRequest() method.
    }
}
