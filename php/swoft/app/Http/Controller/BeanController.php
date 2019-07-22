<?php declare(strict_types=1);


namespace App\Http\Controller;

use App\Model\Logic\RequestBean;
use ReflectionException;
use Swoft\Bean\Annotation\Mapping\Bean;
use Swoft\Bean\BeanFactory;
use Swoft\Bean\Exception\ContainerException;
use Swoft\Co;
use Swoft\Http\Server\Annotation\Mapping\Controller;
use Swoft\Http\Server\Annotation\Mapping\RequestMapping;

/**
 * Class BeanController
 *
 * @since 2.0
 *
 * @Controller(prefix="bean")
 */
class BeanController
{
    /**
     * @RequestMapping()
     *
     * @return array
     * @throws ReflectionException
     * @throws ContainerException
     */
    public function request(): array
    {
        $id = (string)Co::tid();

        /* @var RequestBean $request*/
        $request = BeanFactory::getRequestBean('requestBean', $id);
        return $request->getData();
    }
}