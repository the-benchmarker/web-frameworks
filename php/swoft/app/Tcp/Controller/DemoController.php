<?php declare(strict_types=1);

namespace App\Tcp\Controller;

use Swoft\Tcp\Server\Annotation\Mapping\TcpController;
use Swoft\Tcp\Server\Annotation\Mapping\TcpMapping;
use Swoft\Tcp\Server\Request;
use Swoft\Tcp\Server\Response;

/**
 * Class DemoController
 *
 * @TcpController()
 */
class DemoController
{
    /**
     * @TcpMapping("list", root=true)
     * @param Response $response
     */
    public function list(Response $response): void
    {
        $response->setData('[list]allow command: list, echo, demo.echo');
    }

    /**
     * @TcpMapping("echo")
     * @param Request  $request
     * @param Response $response
     */
    public function index(Request $request, Response $response): void
    {
        $str = $request->getPackage()->getDataString();

        $response->setData('[demo.echo]hi, we received your message: ' . $str);
    }

    /**
     * @TcpMapping("strrev", root=true)
     * @param Request  $request
     * @param Response $response
     */
    public function strRev(Request $request, Response $response): void
    {
        $str = $request->getPackage()->getDataString();

        $response->setData(\strrev($str));
    }

    /**
     * @TcpMapping("echo", root=true)
     * @param Request  $request
     * @param Response $response
     */
    public function echo(Request $request, Response $response): void
    {
        $str = $request->getRawData();

        $response->setData('[echo]hi, we received your message: ' . $str);
    }
}
