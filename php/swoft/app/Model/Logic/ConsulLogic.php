<?php declare(strict_types=1);


namespace App\Model\Logic;

use ReflectionException;
use Swoft\Bean\Annotation\Mapping\Bean;
use Swoft\Bean\Annotation\Mapping\Inject;
use Swoft\Bean\Exception\ContainerException;
use Swoft\Consul\Agent;
use Swoft\Consul\Catalog;
use Swoft\Consul\Exception\ClientException;
use Swoft\Consul\Exception\ServerException;
use Swoft\Consul\Health;
use Swoft\Consul\KV;
use Swoft\Consul\Session;

/**
 * Class ConsulLogic
 *
 * @since 2.0
 *
 * @Bean()
 */
class ConsulLogic
{
    /**
     * @Inject()
     *
     * @var Agent
     */
    private $agent;

    /**
     * @Inject()
     *
     * @var Health
     */
    private $health;

    /**
     * @Inject()
     *
     * @var Catalog
     */
    private $catalog;

    /**
     * @Inject()
     *
     * @var KV
     */
    private $kv;

    /**
     * @Inject()
     *
     * @var Session
     */
    private $session;


    /**
     * @throws ReflectionException
     * @throws ContainerException
     * @throws ClientException
     * @throws ServerException
     */
    public function kv(): void
    {
        $value = 'value content';
        $this->kv->put('/test/my/key', $value);

        $response = $this->kv->get('/test/my/key');
        var_dump($response->getBody(), $response->getResult());
    }
}