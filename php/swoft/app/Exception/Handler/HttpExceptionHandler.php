<?php declare(strict_types=1);

namespace App\Exception\Handler;

use const APP_DEBUG;
use function get_class;
use ReflectionException;
use function sprintf;
use Swoft\Bean\Exception\ContainerException;
use Swoft\Error\Annotation\Mapping\ExceptionHandler;
use Swoft\Http\Message\Response;
use Swoft\Http\Server\Exception\Handler\AbstractHttpErrorHandler;
use Swoft\Log\Helper\CLog;
use Throwable;

/**
 * Class HttpExceptionHandler
 *
 * @ExceptionHandler(\Throwable::class)
 */
class HttpExceptionHandler extends AbstractHttpErrorHandler
{
    /**
     * @param Throwable $e
     * @param Response   $response
     *
     * @return Response
     * @throws ReflectionException
     * @throws ContainerException
     */
    public function handle(Throwable $e, Response $response): Response
    {
        // Log
        CLog::error($e->getMessage());

        // Debug is false
        if (!APP_DEBUG) {
            return $response->withStatus(500)->withContent(
                sprintf(' %s At %s line %d', $e->getMessage(), $e->getFile(), $e->getLine())
            );
        }

        $data = [
            'code'  => $e->getCode(),
            'error' => sprintf('(%s) %s', get_class($e), $e->getMessage()),
            'file'  => sprintf('At %s line %d', $e->getFile(), $e->getLine()),
            'trace' => $e->getTraceAsString(),
        ];

        // Debug is true
        return $response->withData($data);
    }
}
