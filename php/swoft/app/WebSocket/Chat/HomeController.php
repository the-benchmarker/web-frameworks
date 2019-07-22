<?php declare(strict_types=1);

namespace App\WebSocket\Chat;

use Swoft\Session\Session;
use Swoft\WebSocket\Server\Annotation\Mapping\MessageMapping;
use Swoft\WebSocket\Server\Annotation\Mapping\WsController;

/**
 * Class HomeController
 *
 * @WsController()
 */
class HomeController
{
    /**
     * Message command is: 'home.index'
     *
     * @return void
     * @MessageMapping()
     */
    public function index(): void
    {
        Session::mustGet()->push('hi, this is home.index');
    }

    /**
     * Message command is: 'home.echo'
     *
     * @param $data
     * @MessageMapping()
     */
    public function echo($data): void
    {
        Session::mustGet()->push('(home.echo)Recv: ' . $data);
    }

    /**
     * Message command is: 'home.ar'
     *
     * @param $data
     * @MessageMapping("ar")
     *
     * @return string
     */
    public function autoReply($data): string
    {
        return '(home.ar)Recv: ' . $data;
    }
}
