<?php declare(strict_types=1);


namespace App\Listener\Test;


use Swoft\Event\Annotation\Mapping\Listener;
use Swoft\Event\EventHandlerInterface;
use Swoft\Event\EventInterface;
use Swoft\Log\Helper\CLog;
use Swoft\Server\SwooleEvent;

/**
 * Class StartListener
 *
 * @since 2.0
 *
 * @Listener(event=SwooleEvent::START)
 */
class StartListener implements EventHandlerInterface
{
    /**
     * @param EventInterface $event
     */
    public function handle(EventInterface $event): void
    {
        $context = context();

        CLog::info('Start context=' . get_class($context));
    }
}