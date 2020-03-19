<?php
/**
 * @link      http://github.com/zendframework/ZendSkeletonApplication for the canonical source repository
 * @copyright Copyright (c) 2005-2016 Zend Technologies USA Inc. (http://www.zend.com)
 * @license   http://framework.zend.com/license/new-bsd New BSD License
 */

namespace Application\Controller;

use Zend\Mvc\Controller\AbstractActionController;

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
