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

use Silver\Database\Query;
use Silver\Database\Parts\Raw;

class UsersMigrate
{

    protected static $table = 'users';

    public static function up()
    {
        Query::drop(static::$table)->ifExists()->execute();

        Query::create(
            static::$table, function ($q) {
                $q->integer('id')->primary()->autoincrement();
                $q->varchar("username", 255);
                $q->varchar("password", 255);
                $q->varchar("salt", 255);
                $q->varchar("email", 255);
                $q->integer('active')->default(1);
                $q->datetime("create_at", 255)->default(new Raw('CURRENT_TIMESTAMP'));
                $q->datetime("update_at", 255)->nullable();
                $q->datetime("delete_at", 255)->nullable();
            }
        )->execute();

    }

    public static function down() 
    {
        Query::drop(static::$table)->ifExists()->execute();
    }

}
