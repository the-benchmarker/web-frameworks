<?php

declare(strict_types=1);
/**
 * This file is part of Hyperf.
 *
 * @link     https://www.hyperf.io
 * @document https://doc.hyperf.io
 * @contact  group@hyperf.io
 * @license  https://github.com/hyperf/hyperf/blob/master/LICENSE
 */

namespace App\Kernel;

use FastRoute\Dispatcher;
use Hyperf\Contract\NormalizerInterface;
use Hyperf\Di\MethodDefinitionCollectorInterface;
use Hyperf\Dispatcher\HttpDispatcher;
use Hyperf\ExceptionHandler\ExceptionHandlerDispatcher;
use Hyperf\HttpMessage\Server\Response as Psr7Response;
use Hyperf\HttpMessage\Stream\SwooleStream;
use Hyperf\HttpServer\ResponseEmitter;
use Hyperf\HttpServer\Router\Dispatched;
use Hyperf\HttpServer\Router\DispatcherFactory;
use Hyperf\HttpServer\Server;
use Hyperf\Codec\Json;
use Hyperf\Context\Context;
use Psr\Container\ContainerInterface;
use Psr\Http\Message\ResponseInterface;
use Swoole\Http\Request as SwooleRequest;

class FastServer extends Server
{
    protected MethodDefinitionCollectorInterface $methodDefinitionCollector;

    protected NormalizerInterface $normalizer;

    protected Dispatcher $routerDispatcher;

    public function __construct(ContainerInterface $container, HttpDispatcher $dispatcher, ExceptionHandlerDispatcher $exceptionHandlerDispatcher, ResponseEmitter $responseEmitter)
    {
        parent::__construct($container, $dispatcher, $exceptionHandlerDispatcher, $responseEmitter);
        $this->methodDefinitionCollector = $container->get(MethodDefinitionCollectorInterface::class);
        $this->normalizer = $this->container->get(NormalizerInterface::class);
    }

    public function initCoreMiddleware(string $serverName): void
    {
        $this->serverName = $serverName;
        $this->routerDispatcher = $this->createDispatcher($serverName);
    }

    public function onRequest($request, $response): void
    {
        try {
            Context::set(ResponseInterface::class, $psr7Response = new Psr7Response($response));
            $dispatched = new Dispatched(
                $this->routerDispatcher->dispatch(...$this->initMethodAndPath($request))
            );

            if ($dispatched->isFound()) {
                $psr7Response = $this->handleFound($dispatched);
            } else {
                $psr7Response = $this->response()->withStatus(404);
            }
        } catch (Throwable $throwable) {
            // Delegate the exception to exception handler.
            $psr7Response = $this->exceptionHandlerDispatcher->dispatch($throwable, $this->exceptionHandlers);
        } finally {
            $this->responseEmitter->emit($psr7Response, $response, true);
        }
    }

    /**
     * Handle the response when found.
     *
     * @return ResponseInterface
     */
    protected function handleFound(Dispatched $dispatched)
    {
        if ($dispatched->handler->callback instanceof Closure) {
            $response = call($dispatched->handler->callback);
        } else {
            [$controller, $action] = explode('::', $dispatched->handler->callback);
            $controllerInstance = $this->container->get($controller);
            if (!method_exists($controller, $action)) {
                // Route found, but the handler does not exist.
                return $this->response()->withStatus(500)->withBody(new SwooleStream('Method of class does not exist.'));
            }
            $parameters = $this->parseParameters($controller, $action, $dispatched->params);
            $response = $controllerInstance->{$action}(...$parameters);
        }
        if (!$response instanceof ResponseInterface) {
            $response = $this->response()->withStatus(200)
                ->withBody(new SwooleStream(
                    is_array($response) ? Json::encode($response) : (string)$response
                ));
        }
        return $response;
    }

    /**
     * Get response instance from context.
     */
    protected function response(): ResponseInterface
    {
        return Context::get(ResponseInterface::class);
    }

    protected function createDispatcher(string $serverName): Dispatcher
    {
        $factory = $this->container->get(DispatcherFactory::class);
        return $factory->getDispatcher($serverName);
    }

    protected function initMethodAndPath(SwooleRequest $request)
    {
        return [
            $request->server['request_method'] ?? 'GET',
            $request->server['request_uri'] ?? '/',
        ];
    }

    /**
     * Parse the parameters of method definitions, and then bind the specified arguments or
     * get the value from DI container, combine to a argument array that should be injected
     * and return the array.
     */
    protected function parseParameters(string $controller, string $action, array $arguments): array
    {
        $injections = [];
        $definitions = $this->methodDefinitionCollector->getParameters($controller, $action);
        foreach ($definitions ?? [] as $pos => $definition) {
            $value = $arguments[$pos] ?? $arguments[$definition->getMeta('name')] ?? null;
            if ($value === null) {
                if ($definition->getMeta('defaultValueAvailable')) {
                    $injections[] = $definition->getMeta('defaultValue');
                } elseif ($definition->allowsNull()) {
                    $injections[] = null;
                } elseif ($this->container->has($definition->getName())) {
                    $injections[] = $this->container->get($definition->getName());
                } else {
                    throw new \InvalidArgumentException("Parameter '{$definition->getMeta('name')}' "
                        . "of {$controller}::{$action} should not be null");
                }
            } else {
                $injections[] = $this->normalizer->denormalize($value, $definition->getName());
            }
        }

        return $injections;
    }
}
