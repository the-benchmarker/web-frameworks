<?php

declare(strict_types=1);

namespace App\Http\Controller;

use ReflectionException;
use Swoft;
use Swoft\Bean\Exception\ContainerException;
use Swoft\Context\Context;
use Swoft\Http\Message\Response;
use Swoft\Http\Server\Annotation\Mapping\Controller;
use Swoft\Http\Server\Annotation\Mapping\RequestMapping;
use Swoft\Http\Server\Annotation\Mapping\RequestMethod;
use Throwable;

/**
 * Class ApplicationController
 * @Controller()
 */
class ApplicationController
{
    /**
     * @RequestMapping("/", method=RequestMethod::GET)
     * @throws Throwable
     */
    public function index(): Response
    {
        return Context::mustGet()->getResponse()->withContent("");
    }

    /**
     * @RequestMapping("/user/{id}", method=RequestMethod::GET)
     * @param string $id
     *
     * @return Response
     * @throws ReflectionException
     * @throws ContainerException
     */
    public function get(string $id): Response
    {
        return Context::mustGet()->getResponse()->withContent($id);
    }

    /**
     * @RequestMapping("/user", method=RequestMethod::POST)
     *
     * @return Response
     * @throws ReflectionException
     * @throws ContainerException
     */
    public function create(): Response
    {
        return Context::mustGet()->getResponse()->withContent("");
    }
}
