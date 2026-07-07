<?php

namespace app\controllers;

use yii\web\Controller;
use yii\web\Response;

/**
 * Site Controller for Yii2 Benchmark Server
 * 
 * Handles root endpoint for the benchmark server.
 * Follows Yii2 best practices including proper error handling and logging.
 */
class SiteController extends Controller
{
    /**
     * Root endpoint handler
     * 
     * GET /
     * 
     * @return string Empty response for benchmarking
     */
    public function actionIndex()
    {
        \Yii::debug('Root endpoint accessed', 'benchmark');
        
        $response = \Yii::$app->getResponse();
        $response->setStatusCode(200);
        $response->getHeaders()->set('Content-Type', 'text/plain');
        
        return '';
    }

    /**
     * Health check endpoint for monitoring
     * 
     * GET /health
     * 
     * @return string Health status
     */
    public function actionHealth()
    {
        \Yii::debug('Health check endpoint accessed', 'benchmark');
        
        $response = \Yii::$app->getResponse();
        $response->setStatusCode(200);
        $response->getHeaders()->set('Content-Type', 'text/plain');
        
        return 'OK';
    }

    /**
     * Error handler
     * 
     * @return string Error response
     */
    public function actionError()
    {
        \Yii::error('Error occurred: ' . \Yii::$app->errorHandler->exception->getMessage(), 'benchmark');
        
        $response = \Yii::$app->getResponse();
        $response->setStatusCode(500);
        $response->getHeaders()->set('Content-Type', 'text/plain');
        
        return 'Internal Server Error';
    }
}
