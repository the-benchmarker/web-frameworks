<?php

declare(strict_types=1);

namespace App\Controller;

use Psr\Http\Message\ResponseFactoryInterface;
use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use Sunrise\Http\Router\Annotation\GetRoute;
use Sunrise\Http\Router\Annotation\PostRoute;

final readonly class BenchmarkController
{
    public function __construct(
        private ResponseFactoryInterface $responseFactory,
    ) {
    }

    #[GetRoute('home', '/')]
    public function home(): ResponseInterface
    {
        return $this->responseFactory->createResponse();
    }

    #[PostRoute('users.createUser', '/user')]
    public function createUser(): ResponseInterface
    {
        return $this->responseFactory->createResponse();
    }

    #[GetRoute('users.readUserById', '/user/{userId}')]
    public function readUserById(ServerRequestInterface $request): ResponseInterface
    {
        /** @var string $userId */
        $userId = $request->getAttribute('userId');

        $response = $this->responseFactory->createResponse();
        $response->getBody()->write($userId);

        return $response;
    }
}
