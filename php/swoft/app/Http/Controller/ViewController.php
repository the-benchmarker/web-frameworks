<?php declare(strict_types=1);


namespace App\Http\Controller;

use ReflectionException;
use Swoft\Bean\Exception\ContainerException;
use Swoft\Http\Message\ContentType;
use Swoft\Http\Message\Response;
use Swoft\Http\Server\Annotation\Mapping\Controller;
use Swoft\Http\Server\Annotation\Mapping\RequestMapping;
use Swoft\View\Annotation\Mapping\View;
use Throwable;

/**
 * Class ViewController
 *
 * @since 2.0
 *
 * @Controller(prefix="view")
 */
class ViewController
{
    /**
     * @RequestMapping("index")
     *
     * @param Response $response
     *
     * @return Response
     * @throws ReflectionException
     * @throws ContainerException
     */
    public function index(Response $response): Response
    {
        $response = $response->withContent('<html lang="en"><h1>Swoft framework</h1></html>');
        $response = $response->withContentType(ContentType::HTML);
        return $response;
    }

    /**
     * Will render view by annotation tag View
     *
     * @RequestMapping("/home")
     * @View("home/index")
     *
     * @throws Throwable
     */
    public function indexByViewTag(): array
    {
        return [
            'msg' => 'hello'
        ];
    }

    /**
     * @RequestMapping()
     *
     * @return array
     */
    public function ary(): array
    {
        return ['ary'];
    }

    /**
     * @RequestMapping()
     *
     * @return string
     */
    public function str(): string
    {
        return 'string';
    }
}
