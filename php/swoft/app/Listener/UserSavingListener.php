<?php declare(strict_types=1);


namespace App\Listener;

use App\Model\Entity\User;
use Swoft\Event\Annotation\Mapping\Listener;
use Swoft\Event\EventHandlerInterface;
use Swoft\Event\EventInterface;

/**
 * Class UserSavingListener
 *
 * @since 2.0
 *
 * @Listener("swoft.model.user.saving")
 */
class UserSavingListener implements EventHandlerInterface
{
    /**
     * @param EventInterface $event
     */
    public function handle(EventInterface $event): void
    {

        /* @var User $user */
        $user = $event->getTarget();

        /**
            if ($user->getAge() > 100) {
            // stopping saving
            $event->stopPropagation(true);

            $user->setAdd(100);
            }
         */
    }
}
