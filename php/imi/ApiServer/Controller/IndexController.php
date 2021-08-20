<?php
namespace ImiApp\ApiServer\Controller;

use Imi\RequestContext;
use Imi\Controller\HttpController;
use Imi\Server\View\Annotation\View;
use Imi\Server\Route\Annotation\Route;
use Imi\Server\Route\Annotation\Action;
use Imi\Server\Route\Annotation\Controller;

/**
 * @Controller
 * @View(renderType="html")
 */
class IndexController extends HttpController
{
    /**
     * @Action
     * @Route(url="/", method="GET")
     */
    public function index() : string
    {
        return RequestContext::get('response')->write('');
    }

    /**
     * @Action
     * @Route(url="/user/{id}", method="GET")
     */
    public function get($id) : string
    {
        return RequestContext::get('response')->write($id);
    }

    /**
     * @Action
     * @Route(url="/user", method="POST")
     */
    public function create() : string
    {
        return RequestContext::get('response')->write('');
    }
}
