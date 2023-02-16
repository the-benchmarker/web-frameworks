<?php

declare(strict_types=1);

namespace Application\Controller;

use Laminas\Mvc\Controller\AbstractActionController;

class IndexController extends AbstractActionController
{
    public function indexAction()
    {
        return $this->getResponse()->setContent('');
    }

    public function userIdAction()
    {
        return $this->getResponse()->setContent($this->params()->fromRoute('id'));
    }

    public function userAction()
    {
        return $this->getResponse()->setContent('');
    }
}
