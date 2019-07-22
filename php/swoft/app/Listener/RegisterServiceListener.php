<?php declare(strict_types=1);


namespace App\Listener;


use Swoft\Bean\Annotation\Mapping\Inject;
use Swoft\Co;
use Swoft\Consul\Agent;
use Swoft\Event\Annotation\Mapping\Listener;
use Swoft\Event\EventHandlerInterface;
use Swoft\Event\EventInterface;
use Swoft\Http\Server\HttpServer;
use Swoft\Log\Helper\CLog;
use Swoft\Server\SwooleEvent;
use Swoole\Coroutine;

/**
 * Class RegisterServiceListener
 *
 * @since 2.0
 *
 * @Listener(event=SwooleEvent::START)
 */
class RegisterServiceListener implements EventHandlerInterface
{
    /**
     * @Inject()
     *
     * @var Agent
     */
    private $agent;

    /**
     * @param EventInterface $event
     */
    public function handle(EventInterface $event): void
    {
        /* @var HttpServer $httpServer */
        $httpServer = $event->getTarget();

        $service = [
            'ID'                => 'swoft',
            'Name'              => 'swoft',
            'Tags'              => [
                'http'
            ],
            'Address'           => '127.0.0.1',
            'Port'              => $httpServer->getPort(),
            'Meta'              => [
                'version' => '1.0'
            ],
            'EnableTagOverride' => false,
            'Weights'           => [
                'Passing' => 10,
                'Warning' => 1
            ]
        ];


        // Register
//        $this->agent->registerService($service);
//        CLog::info('Swoft http register service success by consul!');

    }
}