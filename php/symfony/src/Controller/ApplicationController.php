<?php

namespace App\Controller;

use Symfony\Bundle\FrameworkBundle\Controller\Controller;
use Symfony\Component\HttpFoundation\Response;
use Sensio\Bundle\FrameworkExtraBundle\Configuration\Route;
use Sensio\Bundle\FrameworkExtraBundle\Configuration\Method;

class ApplicationController extends Controller
{
    /**
     * @Route("/")
     * @Method("GET")
     */
    public function ping()
    {
        return new Response("");
    }

    /**
     * @Route("/user/{id}")
     * @Method("GET")
     */
    public function user($id)
    {
        return new Response($id);
    }

    /**
     * @Route("/user")
     * @Method("POST")
     */
    public function create()
    {
        return new Response("");
    }

}
