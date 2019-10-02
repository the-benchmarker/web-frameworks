<?php
/**
 * Spiral Framework.
 *
 * @license   MIT
 * @author    Anton Titov (Wolfy-J)
 */
declare(strict_types=1);

namespace App\Bootloader;

use App\Controller\BenchmarkController;
use Spiral\Boot\Bootloader\Bootloader;
use Spiral\Router\Route;
use Spiral\Router\RouterInterface;
use Spiral\Router\Target\Action;

class RoutesBootloader extends Bootloader
{
    /**
     * @param RouterInterface $router
     */
    public function boot(RouterInterface $router)
    {
        $router->addRoute(
            'index',
            (new Route('/', new Action(BenchmarkController::class, 'index')))->withVerbs('GET')
        );

        $router->addRoute(
            'benchmark',
            new Route('/user[/<id>]', new Action(BenchmarkController::class, 'user'))
        );
    }
}
