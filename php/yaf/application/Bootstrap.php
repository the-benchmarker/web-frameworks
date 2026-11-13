<?php

class Bootstrap extends Yaf_Bootstrap_Abstract
{
    public function _initView(Yaf_Dispatcher $dispatcher)
    {
        $dispatcher->disableView();
    }

    public function _initRoute(Yaf_Dispatcher $dispatcher)
    {
        $dispatcher->getRouter()->addRoute(
            'user',
            new Yaf_Route_Rewrite('/user/:id', ['controller' => 'User', 'action' => 'show'])
        );
    }
}
