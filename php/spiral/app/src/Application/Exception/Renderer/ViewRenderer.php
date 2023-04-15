<?php

declare(strict_types=1);

namespace App\Application\Exception\Renderer;

use Psr\Http\Message\ResponseFactoryInterface;
use Psr\Http\Message\ResponseInterface;
use Psr\Http\Message\ServerRequestInterface as Request;
use Spiral\Exceptions\Verbosity;
use Spiral\Http\ErrorHandler\RendererInterface;
use Spiral\Http\Header\AcceptHeader;
use Spiral\Views\Exception\ViewException;
use Spiral\Views\ViewsInterface;

/**
 * This Renderer is to allow exceptions to be rendered as HTTP responses, either in JSON format if the request
 * accepts JSON, or as a view if the request accepts HTML.
 * This renderer is intended to be used exclusively by the {@see \Spiral\Http\Middleware\ErrorHandlerMiddleware}
 * middleware and will only be active if the DEBUG environment variable is set to false.
 *
 * @link https://spiral.dev/docs/http-errors
 */
final class ViewRenderer implements RendererInterface
{
    private const DEFAULT_VIEW = 'exception/error';
    private const VIEW_PATTERN = 'exception/%s';

    public function __construct(
        private readonly ViewsInterface $views,
        private readonly ResponseFactoryInterface $responseFactory,
        private readonly Verbosity $verbosity,
    ) {
    }

    public function renderException(Request $request, int $code, \Throwable $exception): ResponseInterface
    {
        // If request accepts json, we will render as a json response
        $acceptItems = AcceptHeader::fromString($request->getHeaderLine('Accept'))->getAll();
        if ($acceptItems && $acceptItems[0]->getValue() === 'application/json') {
            return $this->renderJson($code, $exception);
        }

        return $this->renderView($code, $exception);
    }

    private function renderJson(int $code, \Throwable $exception): ResponseInterface
    {
        $response = $this->responseFactory->createResponse($code);

        $response = $response->withHeader('Content-Type', 'application/json; charset=UTF-8');

        $payload = [
            'status' => $code,
            'error' => $exception->getMessage(),
        ];

        if ($this->verbosity->value > Verbosity::BASIC->value) {
            $payload['stacktrace'] = $exception->getTraceAsString();
        }

        $response->getBody()->write(\json_encode($payload));

        return $response;
    }

    private function renderView(int $code, \Throwable $exception): ResponseInterface
    {
        $response = $this->responseFactory->createResponse($code);

        try {
            // Try to find view for specific exception code
            $view = $this->views->get(\sprintf(self::VIEW_PATTERN, $code));
        } catch (ViewException) {
            // Fallback to default view in case if specific view not found
            $view = $this->views->get(self::DEFAULT_VIEW);
        }

        $content = $view->render(['code' => $code, 'exception' => $exception]);
        $response->getBody()->write($content);

        return $response;
    }
}
