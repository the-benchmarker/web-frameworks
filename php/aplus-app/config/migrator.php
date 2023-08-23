<?php
/*
 * This file is part of App Project.
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
/**
 * Migrator config.
 *
 * @see App::migrator()
 * @see https://docs.aplus-framework.com/guides/libraries/mvc/index.html#migrator-service
 */

return [
    'default' => [
        'directories' => [
            APP_DIR . 'Migrations',
        ],
        'table' => 'Migrations',
        'database_instance' => 'default',
    ],
];
