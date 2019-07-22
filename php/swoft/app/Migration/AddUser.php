<?php declare(strict_types=1);


namespace App\Migration;


use ReflectionException;
use Swoft\Bean\Exception\ContainerException;
use Swoft\Db\Exception\DbException;
use Swoft\Db\Schema;
use Swoft\Db\Schema\Blueprint;
use Swoft\Devtool\Annotation\Mapping\Migration;
use Swoft\Devtool\Migration\Contract\MigrationInterface;

/**
 * Class AddUser20190627225524
 *
 * @since 2.0
 *
 * @Migration(20190627225524)
 */
class AddUser implements MigrationInterface
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
        Schema::createIfNotExists('users', function (Blueprint $blueprint) {
            $blueprint->increments('id');
            $blueprint->smallInteger('age');
            $blueprint->string('label', 10);
            $blueprint->integer('balance');
        });

        Schema::getSchemaBuilder('db.pool')->table('user', function (Blueprint $blueprint) {
            $blueprint->comment('base user tables');
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
        Schema::dropIfExists('users');

        Schema::getSchemaBuilder('db.pool')->dropIfExists('users');
    }
}
