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

class BenchmarkController implements SingletonInterface
{
    public function index()
    {
        return '';
    }

    public function user($id = null)
    {
        return (string)$id;
    }
}
