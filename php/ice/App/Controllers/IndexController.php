<?php

namespace App\Controllers;

class IndexController extends \Ice\Mvc\Controller
{
    public function indexAction()
    {
    }

    public function getUserAction($id)
    {
        echo $id;
    }

    public function postUserAction()
    {
    }
}
