<?php

/**
 * SilverEngine  - PHP MVC framework
 *
 * @package   SilverEngine
 * @author    SilverEngine Team
 * @copyright 2015-2017
 * @license   MIT
 * @link      https://github.com/SilverEngine/Framework
 */

namespace Database\Migrations;

use Silver\Database\Parts\Raw;
use Silver\Database\Query;

class Migrations
{
    private static $table = 'migrations';

    public static function up()
    {
        Query::drop(static::$table)->ifExists()->execute();

        Query::create(
            static::$table, function ($q) {
                $q->integer('id')->primary()->autoincrement();
                $q->varchar('model_name', 100);
                $q->datetime("create_at", 255)->default(new Raw('CURRENT_TIMESTAMP'));
            }
        )->execute();
    }

    public static function down()
    {
        Query::drop(static::$table)->ifExists()->execute();
    }
}
