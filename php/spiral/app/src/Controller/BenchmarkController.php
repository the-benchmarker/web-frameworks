<?php
/**
 * Spiral Framework.
 *
 * @license   MIT
 * @author    Anton Titov (Wolfy-J)
 */
declare(strict_types=1);

namespace App\Controller;

use Nyholm\Psr7\Response;
use Spiral\Core\Container\SingletonInterface;

final class BenchmarkController implements SingletonInterface
{

    /** @var Response */
    private $plain;

    public function __construct() {

        $this->plain = new Response(200, ['Content-Type' => 'text/plain']);
        $this->plain->getBody()->write('');
        return $this->plain;
    }
}