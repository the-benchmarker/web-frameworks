<?php declare(strict_types=1);


namespace App\Listener\Test;


use Swoft\Event\Annotation\Mapping\Listener;
use Swoft\Event\EventHandlerInterface;
use Swoft\Event\EventInterface;
use Swoft\Log\Helper\CLog;
use Swoft\Server\SwooleEvent;

/**
 * Class ShutDownListener
 *
 * @since 2.0
 *
 * @Listener(SwooleEvent::SHUTDOWN)
 */
class ShutDownListener implements EventHandlerInterface
{
    /**
     * @param EventInterface $event
     */
    public function handle(EventInterface $event): void
    {
        $context = context();

        CLog::info(' Shut down context=' . get_class($context));
    }
}