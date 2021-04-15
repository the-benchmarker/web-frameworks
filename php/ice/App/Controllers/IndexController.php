<?php

namespace App\Controllers;

class IndexController extends \Ice\Mvc\Controller
{
    public function indexAction()
    {
    }

    public function getUserAction()
    {
        echo $this->dispatcher->getParam('id');
    }

    public function postUserAction()
    {
    }
}
