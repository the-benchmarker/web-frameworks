<?php declare(strict_types=1);


namespace App\Http\Controller;

use Swoft\Db\Schema\Blueprint;
use Swoft\Db\Schema;
use Swoft\Http\Server\Annotation\Mapping\Controller;
use Swoft\Http\Server\Annotation\Mapping\RequestMapping;

/**
 * Class DbBuilderController
 *
 * @since 2.0
 *
 * @Controller("builder")
 */
class DbBuilderController
{

    /**
     * @RequestMapping()
     *
     * @return void
     */
    public function schema()
    {
        Schema::createIfNotExists('test', function (Blueprint $blueprint) {
            $blueprint->increments('id');
        });
    }
}
