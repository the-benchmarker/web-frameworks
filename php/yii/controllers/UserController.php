<?php

namespace app\controllers;

use yii\web\Controller;

/**
 * User Controller for Yii2 Benchmark Server
 * 
 * Handles user-related endpoints for the benchmark server.
 * Follows Yii2 best practices including proper error handling and logging.
 */
class UserController extends Controller
{
    /**
     * @var bool Disable CSRF validation for benchmarking
     */
    public $enableCsrfValidation = false;

    /**
     * Create a new user
     * 
     * POST /user
     * 
     * @return string Empty response for benchmarking
     */
    public function actionIndex()
    {
        \Yii::debug('Create user endpoint accessed', 'benchmark');
        
        $response = \Yii::$app->getResponse();
        $response->setStatusCode(200);
        $response->getHeaders()->set('Content-Type', 'text/plain');
        
        return '';
    }

    /**
     * Get user by ID
     * 
     * GET /user/{id}
     * 
     * @param string $id User identifier
     * @return string User ID as plain text
     */
    public function actionSearch($id)
    {
        \Yii::debug("User endpoint accessed with ID: {$id}", 'benchmark');
        
        $response = \Yii::$app->getResponse();
        $response->setStatusCode(200);
        $response->getHeaders()->set('Content-Type', 'text/plain');
        
        return $id;
    }
}
