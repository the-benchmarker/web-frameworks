<?php declare(strict_types=1);


namespace App\Rpc\Service;


use App\Rpc\Lib\UserInterface;
use Exception;
use Swoft\Co;
use Swoft\Rpc\Server\Annotation\Mapping\Service;

/**
 * Class UserService
 *
 * @since 2.0
 *
 * @Service()
 */
class UserService implements UserInterface
{
    /**
     * @param int   $id
     * @param mixed $type
     * @param int   $count
     *
     * @return array
     */
    public function getList(int $id, $type, int $count = 10): array
    {
        return ['name' => ['list']];
    }

    /**
     * @param int $id
     *
     * @return bool
     */
    public function delete(int $id): bool
    {
        return false;
    }

    /**
     * @return void
     */
    public function returnNull(): void
    {
        return;
    }

    /**
     * @return string
     */
    public function getBigContent(): string
    {
        $content = Co::readFile(__DIR__ . '/big.data');
        return $content;
    }

    /**
     * Exception
     * @throws Exception
     */
    public function exception(): void
    {
        throw new Exception('exception version');
    }

    /**
     * @param string $content
     *
     * @return int
     */
    public function sendBigContent(string $content): int
    {
        return strlen($content);
    }
}