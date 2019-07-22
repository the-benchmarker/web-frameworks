<?php declare(strict_types=1);


namespace App\Task\Task;

use Swoft\Task\Annotation\Mapping\Task;
use Swoft\Task\Annotation\Mapping\TaskMapping;

/**
 * Class TestTask
 *
 * @since 2.0
 *
 * @Task(name="testTask")
 */
class TestTask
{
    /**
     * @TaskMapping(name="list")
     *
     * @param int    $id
     * @param string $default
     *
     * @return array
     */
    public function getList(int $id, string $default = 'def'): array
    {
        return [
            'list'    => [1, 3, 3],
            'id'      => $id,
            'default' => $default
        ];
    }

    /**
     * @TaskMapping()
     *
     * @param int $id
     *
     * @return bool
     */
    public function delete(int $id): bool
    {
        if ($id > 10) {
            return true;
        }

        return false;
    }

    /**
     * @TaskMapping()
     *
     * @param string $name
     *
     * @return null
     */
    public function returnNull(string $name)
    {
        return null;
    }

    /**
     * @TaskMapping()
     *
     * @param string $name
     */
    public function returnVoid(string $name): void
    {
        return;
    }
}