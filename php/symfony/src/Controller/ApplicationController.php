<?php

namespace App\Controller;

use Symfony\Component\HttpFoundation\Response;
use Symfony\Component\Routing\Annotation\Route;

class ApplicationController
{
    /**
     * @Route("/", methods={"GET"})
     */
    public function ping()
    {
        return new Response('');
    }

    /**
     * @Route("/user/{id}", methods={"GET"})
     */
    public function user($id)
    {
        return new Response($id);
    }

    /**
     * @Route("/user", methods={"POST"})
     */
    public function create()
    {
        return new Response('');
    }
}
