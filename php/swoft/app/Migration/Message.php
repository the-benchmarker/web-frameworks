<?php declare(strict_types=1);


namespace App\Migration;


use ReflectionException;
use Swoft\Bean\Exception\ContainerException;
use Swoft\Db\Exception\DbException;
use Swoft\Db\Schema\Blueprint;
use Swoft\Devtool\Annotation\Mapping\Migration;
use Swoft\Devtool\Migration\Migration as BaseMigration;

/**
 * Class Message20190627225525
 *
 * @since 2.0
 *
 * @Migration(time=20190627225525, pool="db3.pool")
 */
class Message extends BaseMigration
{
    /**
     * @return void
     *
     * @throws ReflectionException
     * @throws ContainerException
     * @throws DbException
     */
    public function up(): void
    {
        $this->schema->createIfNotExists('messages', function (Blueprint $blueprint) {
            $blueprint->increments('id');
            $blueprint->text('content');
            $blueprint->timestamps();
        });
    }

    /**
     * @return void
     *
     * @throws ReflectionException
     * @throws ContainerException
     * @throws DbException
     */
    public function down(): void
    {
        $this->schema->dropIfExists('messages');
    }
}
