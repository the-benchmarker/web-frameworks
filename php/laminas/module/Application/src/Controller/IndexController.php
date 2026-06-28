<?php

declare(strict_types=1);

namespace Application\Controller;

use Laminas\Mvc\Controller\AbstractActionController;

/**
 * Index Controller for Laminas Benchmark Server
 * 
 * Handles all benchmark endpoints for the Laminas framework.
 * Follows Laminas best practices including proper error handling and logging.
 */
class IndexController extends AbstractActionController
{
    /**
     * Root endpoint handler
     * 
     * GET /
     * 
     * @return mixed Empty response for benchmarking
     */
    public function indexAction()
    {
        error_log('[' . date('Y-m-d H:i:s') . '] DEBUG - Root endpoint accessed');
        
        $response = $this->getResponse();
        $response->setContent('');
        $response->getHeaders()->addHeaderLine('Content-Type', 'text/plain');
        
        return $response;
    }

    /**
     * Get user by ID
     * 
     * GET /user/{id}
     * 
     * @return mixed User ID as plain text
     */
    public function userIdAction()
    {
        $id = $this->params()->fromRoute('id');
        error_log('[' . date('Y-m-d H:i:s') . '] DEBUG - User endpoint accessed with ID: ' . $id);
        
        $response = $this->getResponse();
        $response->setContent($id);
        $response->getHeaders()->addHeaderLine('Content-Type', 'text/plain');
        
        return $response;
    }

    /**
     * Create new user
     * 
     * POST /user
     * 
     * @return mixed Empty response for benchmarking
     */
    public function userAction()
    {
        error_log('[' . date('Y-m-d H:i:s') . '] DEBUG - Create user endpoint accessed');
        
        $response = $this->getResponse();
        $response->setContent('');
        $response->getHeaders()->addHeaderLine('Content-Type', 'text/plain');
        
        return $response;
    }

    /**
     * Health check endpoint for monitoring
     * 
     * GET /health
     * 
     * @return mixed Health status
     */
    public function healthAction()
    {
        error_log('[' . date('Y-m-d H:i:s') . '] DEBUG - Health check endpoint accessed');
        
        $response = $this->getResponse();
        $response->setContent('OK');
        $response->getHeaders()->addHeaderLine('Content-Type', 'text/plain');
        
        return $response;
    }
}
