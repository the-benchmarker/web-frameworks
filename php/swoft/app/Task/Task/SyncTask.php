<?php declare(strict_types=1);


namespace App\Task\Task;

use Swoft\Task\Annotation\Mapping\Task;
use Swoft\Task\Annotation\Mapping\TaskMapping;

/**
 * Class SyncTask
 *
 * @since 2.0
 *
 * @Task(name="sync")
 */
class SyncTask
{
    /**
     * @TaskMapping()
     *
     * @param string $name
     *
     * @return string
     */
    public function test(string $name): string
    {
        return 'sync-test-' . $name;
    }

    /**
     * @TaskMapping()
     *
     * @return bool
     */
    public function testBool(): bool
    {
        return true;
    }

    /**
     * @TaskMapping()
     *
     * @return bool
     */
    public function testNull()
    {
        return null;
    }
}