<?php declare(strict_types=1);


namespace App\Rpc\Lib;

/**
 * Class UserInterface
 *
 * @since 2.0
 */
interface UserInterface
{
    /**
     * @param int   $id
     * @param mixed $type
     * @param int   $count
     *
     * @return array
     */
    public function getList(int $id, $type, int $count = 10): array;

    /**
     * @param int $id
     *
     * @return bool
     */
    public function delete(int $id): bool;

    /**
     * @return string
     */
    public function getBigContent(): string;

    /**
     * @return void
     */
    public function returnNull():void ;

    /**
     * Exception
     */
    public function exception(): void;

    /**
     * @param string $content
     *
     * @return int
     */
    public function sendBigContent(string $content): int;
}