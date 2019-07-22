<?php declare(strict_types=1);


namespace App\Listener;

use App\Model\Entity\User;
use Swoft\Db\DbEvent;
use Swoft\Db\Eloquent\Model;
use Swoft\Event\Annotation\Mapping\Listener;
use Swoft\Event\EventHandlerInterface;
use Swoft\Event\EventInterface;

/**
 * Class RanListener
 *
 * @since 2.0
 *
 * @Listener(DbEvent::MODEL_SAVED)
 */
class ModelSavedListener implements EventHandlerInterface
{
    /**
     * @param EventInterface $event
     */
    public function handle(EventInterface $event): void
    {
        /* @var Model $modelStatic */
        $modelStatic = $event->getTarget();

        if ($modelStatic instanceof User) {
            // to do something....
        }

        // ....
    }
}
