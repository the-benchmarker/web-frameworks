<?php

/*
 * This file is part of the DriftPHP package.
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 *
 * Feel free to edit as you please, and have fun.
 *
 * @author Marc Morera <yuhu@mmoreram.com>
 */

declare(strict_types=1);

namespace App\Controller;

use React\Http\Message\Response;
use React\Promise\PromiseInterface;

use function React\Promise\resolve;

/**
 * Class DefaultController
 */
class DefaultController
{
    /**
     * @return PromiseInterface
     */
    public function index(): PromiseInterface
    {
        return resolve(new Response());
    }

    /**
     * @param string $userId
     *
     * @return PromiseInterface
     */
    public function getUser(string $userId): PromiseInterface
    {
        return resolve(new Response(200, [], $userId));
    }

    /**
     * @return PromiseInterface
     */
    public function createUser(): PromiseInterface
    {
        return resolve(new Response());
    }
}
