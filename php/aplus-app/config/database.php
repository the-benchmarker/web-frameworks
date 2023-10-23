<?php
/*
 * This file is part of App Project.
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
/**
 * Database config.
 *
 * @see App::database()
 * @see Framework\Database\Database::makeConfig()
 * @see https://docs.aplus-framework.com/guides/libraries/mvc/index.html#database-service
 */
return [
    'default' => [
        'config' => [
            'username' => 'root',
            'password' => 'password',
            'schema' => 'framework-tests',
            'host' => 'localhost',
        ],
        'logger_instance' => 'default',
    ],
];
