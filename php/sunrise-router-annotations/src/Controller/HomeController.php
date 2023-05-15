<?php

declare(strict_types=1);

namespace App\Controller;

use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface;
use Psr\Http\Server\RequestHandlerInterface;
use Sunrise\Http\Message\ResponseFactory;
use Sunrise\Http\Router\Annotation\Route;

/** @Route(name="home", path="/", method="GET") */
#[Route(name: 'home', path: '/', method: 'GET')]
final class HomeController implements RequestHandlerInterface
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
