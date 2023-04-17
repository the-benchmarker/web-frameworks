<?php

declare(strict_types=1);

namespace App\Controller\User;

use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use Psr\Http\Server\RequestHandlerInterface;
use Sunrise\Http\Message\ResponseFactory;
use Sunrise\Http\Router\Annotation\Route;

/** @Route(name="user.create", path="/user", method="POST") */
#[Route(name: 'user.create', path: '/user', method: 'POST')]
final class UserCreateController implements RequestHandlerInterface
{
    /**
     * {@inheritdoc}
     *
     * @param ServerRequestInterface $request
     *
     * @return ResponseInterface
     */
    public function handle(ServerRequestInterface $request): ResponseInterface
    {
        return (new ResponseFactory())->createResponse(200);
    }
}
