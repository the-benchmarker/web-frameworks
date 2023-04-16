<?php

namespace app\controller;

use kuiper\di\attribute\Controller;
use kuiper\web\AbstractController;
use kuiper\web\attribute\GetMapping;
use kuiper\web\attribute\PostMapping;
use Psr\Http\Message\ResponseInterface;

#[Controller]
class IndexController extends AbstractController
{
    #[GetMapping("/")]
    public function index(): void
    {
    }

    #[GetMapping("/user/{id:\d+}")]
    public function userGet(int $id): void
    {
        $this->getResponse()->getBody()->write((string) $id);
    }

    #[PostMapping("/user")]
    public function userCreate(): void
    {
    }
}
