<?php

declare(strict_types=1);

namespace App\Controller\User;

use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use Psr\Http\Server\RequestHandlerInterface;
use Sunrise\Http\Message\ResponseFactory;
use Sunrise\Http\Router\Annotation\Route;

/** @Route(name="user.read", path="/user/{id}", method="GET") */
#[Route(name: 'user.read', path: '/user/{id}', method: 'GET')]
final class UserReadController implements RequestHandlerInterface
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
        $response = (new ResponseFactory())->createResponse(200);
        $response->getBody()->write($request->getAttribute('id'));

        return $response;
    }
}
