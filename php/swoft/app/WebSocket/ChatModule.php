<?php declare(strict_types=1);

namespace App\WebSocket;

use App\WebSocket\Chat\HomeController;
use Swoft\Http\Message\Request;
use Swoft\WebSocket\Server\Annotation\Mapping\OnOpen;
use Swoft\WebSocket\Server\Annotation\Mapping\WsModule;
use Swoft\WebSocket\Server\MessageParser\TokenTextParser;
use function server;

/**
 * Class ChatModule
 *
 * @WsModule(
 *     "/chat",
 *     messageParser=TokenTextParser::class,
 *     controllers={HomeController::class}
 * )
 */
class ChatModule
{
    /**
     * @OnOpen()
     * @param Request $request
     * @param int     $fd
     */
    public function onOpen(Request $request, int $fd): void
    {
        server()->push($request->getFd(), "Opened, welcome!(FD: $fd)");
    }
}
