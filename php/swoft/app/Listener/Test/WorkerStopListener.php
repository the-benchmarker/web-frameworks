<?php declare(strict_types=1);


namespace App\Listener\Test;


use Swoft\Event\Annotation\Mapping\Listener;
use Swoft\Event\EventHandlerInterface;
use Swoft\Event\EventInterface;
use Swoft\Log\Helper\CLog;
use Swoft\Server\SwooleEvent;

/**
 * Class WorkerStopListener
 *
 * @since 2.0
 *
 * @Listener(SwooleEvent::WORKER_STOP)
 */
class WorkerStopListener implements EventHandlerInterface
{
    /**
     * @param EventInterface $event
     */
    public function handle(EventInterface $event): void
    {
        $context = context();

        CLog::info('Worker Start context=' . get_class($context));
    }
}