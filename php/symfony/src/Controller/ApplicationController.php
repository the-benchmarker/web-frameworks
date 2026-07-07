<?php

namespace App\Controller;

use Psr\Log\LoggerInterface;
use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;

/**
 * Symfony Benchmark Application Controller
 * 
 * Handles all benchmark endpoints for the Symfony framework.
 * Follows Symfony best practices including proper error handling and logging.
 */
class ApplicationController
{
    /**
     * Logger instance
     * 
     * @var LoggerInterface
     */
    private $logger;

    /**
     * ApplicationController constructor.
     * 
     * @param LoggerInterface $logger
     */
    public function __construct(LoggerInterface $logger)
    {
        $this->logger = $logger;
    }

    /**
     * Root endpoint handler
     * 
     * @Route("/", methods={"GET"})
     * 
     * @return Response Empty response for benchmarking
     */
    public function ping()
    {
        $this->logger->debug('Root endpoint accessed');
        
        return new Response('', 200, ['Content-Type' => 'text/plain']);
    }

    /**
     * Get user by ID
     * 
     * @Route("/user/{id}", methods={"GET"})
     * 
     * @param string $id User identifier
     * @return Response User ID as plain text
     */
    public function user($id)
    {
        $this->logger->debug("User endpoint accessed with ID: {$id}");
        
        return new Response($id, 200, ['Content-Type' => 'text/plain']);
    }

    /**
     * Create new user
     * 
     * @Route("/user", methods={"POST"})
     * 
     * @return Response Empty response for benchmarking
     */
    public function create()
    {
        $this->logger->debug('Create user endpoint accessed');
        
        return new Response('', 200, ['Content-Type' => 'text/plain']);
    }

    /**
     * Health check endpoint for monitoring
     * 
     * @Route("/health", methods={"GET"})
     * 
     * @return Response Health status
     */
    public function healthCheck()
    {
        $this->logger->debug('Health check endpoint accessed');
        
        return new Response('OK', 200, ['Content-Type' => 'text/plain']);
    }
}
