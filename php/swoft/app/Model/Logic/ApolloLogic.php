<?php declare(strict_types=1);


namespace App\Model\Logic;

use Swoft\Apollo\Config;
use Swoft\Apollo\Exception\ApolloException;
use Swoft\Bean\Annotation\Mapping\Bean;
use Swoft\Bean\Annotation\Mapping\Inject;

/**
 * Class ApolloLogic
 *
 * @since 2.0
 *
 * @Bean()
 */
class ApolloLogic
{
    /**
     * @Inject()
     *
     * @var Config
     */
    private $config;

    /**
     * @throws ApolloException
     */
    public function pull(): void
    {
        $data = $this->config->pull('application');

        // Print data
        var_dump($data);
    }
}