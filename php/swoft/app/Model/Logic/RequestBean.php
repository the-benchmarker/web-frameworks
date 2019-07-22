<?php declare(strict_types=1);


namespace App\Model\Logic;

use Swoft\Bean\Annotation\Mapping\Bean;

/**
 * Class RequestBean
 *
 * @since 2.0
 *
 * @Bean(scope=Bean::REQUEST, name="requestBean")
 */
class RequestBean
{
    /**
     * @return array
     */
    public function getData(): array
    {
        return ['requestBean'];
    }

    /**
     * @param string $type
     *
     * @return string
     */
    public function getName(string $type):string {
        return 'name';
    }
}