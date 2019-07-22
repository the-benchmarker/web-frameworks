<?php declare(strict_types=1);


namespace App\Model\Logic;

use Swoft\Bean\Annotation\Mapping\Bean;
use Swoft\Limiter\Annotation\Mapping\RateLimiter;

/**
 * Class LimiterLogic
 *
 * @since 2.0
 *
 * @Bean()
 */
class LimiterLogic
{
    /**
     * @RateLimiter(fallback="limterFallback")
     *
     * @return array
     */
    public function limter(): array
    {
        // Do something

        return [];
    }

    /**
     * @RateLimiter(key="requestBean.getName('name')")
     *
     * @param RequestBean $requestBean
     *
     * @return array
     */
    public function limterParams(RequestBean $requestBean): array
    {
        // Do something

        return [];
    }

    /**
     * @return array
     */
    public function limterFallback(): array
    {
        return [];
    }
}