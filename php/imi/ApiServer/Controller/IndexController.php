<?php
namespace ImiApp\ApiServer\Controller;

use Imi\Server\Http\Controller\HttpController;
use Imi\Server\Http\Route\Annotation\Action;
use Imi\Server\Http\Route\Annotation\Controller;
use Imi\Server\Http\Route\Annotation\Route;

/**
 * @Controller
 */
class IndexController extends HttpController
{
    /**
     * @Action
     * @Route(url="/", method="GET")
     */
    public function index()
    {
        return '';
    }

    /**
     * @Action
     * @Route(url="/user/{id}", method="GET")
     */
    public function get($id)
    {
        return $id;
    }

    /**
     * @Action
     * @Route(url="/user", method="POST")
     */
    public function create()
    {
        return '';
    }
}
