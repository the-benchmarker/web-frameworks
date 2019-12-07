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
     *
     * @return void
     */
    public function index()
    {
        return RequestContext::get('response')->write('');
    }

    /**
     * @Action
     * @Route(url="/user/{id}", method="GET")
     *
     * @return void
     */
    public function get($id)
    {
        return RequestContext::get('response')->write($id);
    }

    /**
     * @Action
     * @Route(url="/user", method="POST")
     *
     * @return void
     */
    public function create()
    {
        return RequestContext::get('response')->write('');
    }
}
