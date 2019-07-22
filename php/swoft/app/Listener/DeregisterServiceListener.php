<?php declare(strict_types=1);


namespace App\Listener;


use ReflectionException;
use Swoft\Bean\Annotation\Mapping\Inject;
use Swoft\Bean\Exception\ContainerException;
use Swoft\Co;
use Swoft\Consul\Agent;
use Swoft\Consul\Exception\ClientException;
use Swoft\Consul\Exception\ServerException;
use Swoft\Event\Annotation\Mapping\Listener;
use Swoft\Event\EventHandlerInterface;
use Swoft\Event\EventInterface;
use Swoft\Http\Server\HttpServer;
use Swoft\Server\SwooleEvent;
use Swoole\Coroutine;

/**
 * Class DeregisterServiceListener
 *
 * @since 2.0
 *
 * @Listener(SwooleEvent::SHUTDOWN)
 */
class DeregisterServiceListener implements EventHandlerInterface
{
    /**
     * @Inject()
     *
     * @var Agent
     */
    private $agent;

    /**
     * @param EventInterface $event
     *
     * @throws ReflectionException
     * @throws ContainerException
     * @throws ClientException
     * @throws ServerException
     */
    public function handle(EventInterface $event): void
    {
        /* @var HttpServer $httpServer */
        $httpServer = $event->getTarget();

//        $this->agent->deregisterService('swoft');
    }
}