<?php
namespace ImiApp\ApiServer\Controller;

use Imi\Server\Http\Route\Annotation\Route;
use Imi\Server\Http\Route\Annotation\Action;
use Imi\Server\Http\Controller\HttpController;
use Imi\Server\Http\Route\Annotation\Controller;
use Imi\Server\Http\Message\Contract\IHttpResponse;

/**
 * @Controller
 */
class IndexController extends HttpController
{
    /**
     * @Action
     * @Route(url="/", method="GET")
     * @return mixed
     */
    public function index(): IHttpResponse
    {
        return $this->response;
    }

    /**
     * @Action
     * @Route(url="/user/{id}", method="GET")
     * @return mixed
     */
    public function get($id): IHttpResponse
    {
        $response = $this->response;
        $response->getBody()->write($id);
        return $response;
    }

    /**
     * @Action
     * @Route(url="/user", method="POST")
     */
    public function create(): IHttpResponse
    {
        return $this->response;
    }
}
