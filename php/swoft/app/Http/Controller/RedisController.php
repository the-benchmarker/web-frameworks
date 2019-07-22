<?php declare(strict_types=1);


namespace App\Http\Controller;

use Exception;
use function sgo;
use Swoft\Bean\Annotation\Mapping\Inject;
use Swoft\Http\Server\Annotation\Mapping\Controller;
use Swoft\Http\Server\Annotation\Mapping\RequestMapping;
use Swoft\Redis\Exception\RedisException;
use Swoft\Redis\Pool;
use Swoft\Redis\Redis;

/**
 * Class RedisController
 *
 * @since 2.0
 * @Controller("redis")
 */
class RedisController
{

    /**
     * @Inject()
     *
     * @var Pool
     */
    private $redis;

    /**
     * @RequestMapping("poolSet")
     */
    public function poolSet(): array
    {
        $key   = 'key';
        $value = uniqid();

        $this->redis->set($key, $value);

        $get = $this->redis->get($key);

        $isError = $this->redis->call(function (\Redis $redis) {
            $redis->eval('returnxxxx 1');

            return $redis->getLastError();
        });

        return [$get, $value, $isError];
    }

    /**
     * @RequestMapping()
     */
    public function set(): array
    {
        $key   = 'key';
        $value = uniqid();

        $this->redis->zAdd($key, [
            'add'    => 11.1,
            'score2' => 11.1,
            'score3' => 11.21
        ]);

        $get = $this->redis->sMembers($key);

        return [$get, $value];
    }


    /**
     * @RequestMapping("str")
     */
    public function str(): array
    {
        $key    = 'key';
        $result = Redis::set($key, 'key');

        $keyVal = Redis::get($key);

        $isError = Redis::call(function (\Redis $redis) {
            $redis->eval('return 1');

            return $redis->getLastError();
        });

        $data = [
            $result,
            $keyVal,
            $isError
        ];

        return $data;
    }

    /**
     * Only to use test. The wrong way to use it
     *
     * @RequestMapping("release")
     *
     * @return array
     * @throws RedisException
     */
    public function release(): array
    {
        sgo(function () {
            Redis::connection();
        });

        Redis::connection();

        return ['release'];
    }

    /**
     * Only to use test. The wrong way to use it
     *
     * @RequestMapping("ep")
     *
     * @return array
     */
    public function exPipeline(): array
    {
        sgo(function () {
            Redis::pipeline(function () {
                throw new Exception('');
            });
        });

        Redis::pipeline(function () {
            throw new Exception('');
        });

        return ['exPipeline'];
    }

    /**
     * Only to use test. The wrong way to use it
     *
     * @RequestMapping("et")
     *
     * @return array
     */
    public function exTransaction(): array
    {
        sgo(function () {
            Redis::transaction(function () {
                throw new Exception('');
            });
        });

        Redis::transaction(function () {
            throw new Exception('');
        });

        return ['exPipeline'];
    }
}
