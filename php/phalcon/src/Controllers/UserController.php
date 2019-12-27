<?php
declare(strict_types=1);


namespace Application\Controllers;

use Phalcon\Mvc\Controller;
use Phalcon\Mvc\Dispatcher;

class UserController extends Controller
{
    public function createAction(): void
    {
        echo "";
    }

    public function showAction(): void
    {
        echo $this->dispatcher->getParam('id');
    }
}
