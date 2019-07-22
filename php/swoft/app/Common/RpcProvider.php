<?php declare(strict_types=1);


namespace App\Common;


use ReflectionException;
use Swoft\Bean\Annotation\Mapping\Bean;
use Swoft\Bean\Annotation\Mapping\Inject;
use Swoft\Bean\Exception\ContainerException;
use Swoft\Consul\Agent;
use Swoft\Consul\Exception\ClientException;
use Swoft\Consul\Exception\ServerException;
use Swoft\Rpc\Client\Client;
use Swoft\Rpc\Client\Contract\ProviderInterface;

/**
 * Class RpcProvider
 *
 * @since 2.0
 *
 * @Bean()
 */
class RpcProvider implements ProviderInterface
{
    /**
     * @Inject()
     *
     * @var Agent
     */
    private $agent;

    /**
     * @param Client $client
     *
     * @return array
     * @throws ReflectionException
     * @throws ContainerException
     * @throws ClientException
     * @throws ServerException
     * @example
     * [
     *     'host:port',
     *     'host:port',
     *     'host:port',
     * ]
     */
    public function getList(Client $client): array
    {
        // Get health service from consul
        $services = $this->agent->services();

        $services = [

        ];

        return $services;
    }
}