<?php declare(strict_types=1);


namespace App\Http\Controller;

use App\Model\Entity\User;
use Exception;
use Swoft\Co;
use Swoft\Http\Server\Annotation\Mapping\Controller;
use Swoft\Http\Server\Annotation\Mapping\RequestMapping;
use Swoft\Redis\Redis;
use Swoole\Coroutine\Http\Client;
use Throwable;

/**
 * Class CoController
 *
 * @since 2.0
 *
 * @Controller()
 */
class CoController
{
    /**
     * @RequestMapping()
     *
     * @return array
     *
     * @throws Exception
     */
    public function multi(): array
    {
        $requests = [
            'addUser' => [$this, 'addUser'],
            'getUser' => "App\Http\Controller\CoController::getUser",
            'curl'    => function () {
                $cli = new Client('127.0.0.1', 18306);
                $cli->get('/redis/str');
                $result = $cli->body;
                $cli->close();

                return $result;
            }
        ];

        $response = Co::multi($requests);

        return $response;
    }

    /**
     * @return array
     */
    public static function getUser(): array
    {
        $result = Redis::set('key', 'value');

        return [$result, Redis::get('key')];
    }

    /**
     * @return array
     * @throws Throwable
     */
    public function addUser(): array
    {
        $user = User::new();
        $user->setAge(mt_rand(1, 100));
        $user->setUserDesc('desc');

        // Save result
        $result = $user->save();

        return [$result, $user->getId()];
    }
}