<?php declare(strict_types=1);


namespace App\Http\Controller;

use Swoft\Context\Context;
use Swoft\Http\Message\Response;
use Swoft\Http\Server\Annotation\Mapping\Controller;
use Swoft\Http\Server\Annotation\Mapping\RequestMapping;

/**
 * Class BeanController
 *
 * @since 2.0
 *
 * @Controller(prefix="resp")
 */
class RespController
{
    /**
     * @RequestMapping()
     *
     * @return Response
     */
    public function cookie(): Response
    {
        /* @var Response $resp */
        $resp = Context::mustGet()->getResponse();

        return $resp->setCookie('c-name', 'c-value')->withData(['hello']);
    }
}
