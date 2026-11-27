<?php

namespace App\Exceptions;

use MacropaySolutions\Framework\Exceptions\Handler as ExceptionHandler;
use MacropaySolutions\Kernel\Auth\Access\AuthorizationException;
use MacropaySolutions\Kernel\Database\Obvious\ModelNotFoundException;
use MacropaySolutions\Kernel\Validation\ValidationException;
use Symfony\Component\HttpKernel\Exception\HttpException;
use Throwable;

class Handler extends ExceptionHandler
{
    /**
     * A list of the exception types that should not be reported.
     *
     * @var array
     */
    protected $dontReport = [
        AuthorizationException::class,
        HttpException::class,
        ModelNotFoundException::class,
        ValidationException::class,
    ];

    /**
     * Report or log an exception.
     *
     * This is a great spot to send exceptions to Sentry, Bugsnag, etc.
     *
     * @throws \Exception
     */
    public function report(Throwable $e): void
    {
        parent::report($e);
    }

    /**
     * Render an exception into an HTTP response.
     *
     * @param  \MacropaySolutions\Kernel\Http\Request  $request
     * @return \MacropaySolutions\Kernel\Http\Response|\MacropaySolutions\Kernel\Http\JsonResponse
     *
     * @throws \Throwable
     */
    public function render($request, Throwable $e)
    {
//        // To return the {message, errors} validation format instead of the flat 422 map:
//         if ($e instanceof ValidationException) {
//             return \app(\MacropaySolutions\Kernel\Http\JsonResponse::class, [
//                 ['message' => $e->getMessage(), 'errors' => $e->errors()],
//                 $e->status,
//             ]);
//         }

        return parent::render($request, $e);
    }
}
