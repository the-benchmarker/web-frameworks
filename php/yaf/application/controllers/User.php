<?php

class UserController extends Yaf_Controller_Abstract
{
    public function indexAction()
    {
    }

    public function showAction($id)
    {
        $this->getResponse()->setBody($id);
    }
}
