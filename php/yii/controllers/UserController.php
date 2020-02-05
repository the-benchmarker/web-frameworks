<?php

namespace app\controllers;

use yii\web\Controller;

class UserController extends Controller
{
    public $layout = false;
    public $enableCsrfValidation = false;

    public function actionIndex()
    {
        return '';
    }

    public function actionSearch($id)
    {
        return $id;
    }
}
