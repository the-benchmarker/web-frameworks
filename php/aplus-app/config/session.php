<?php
/*
 * This file is part of App Project.
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
/**
 * Session config.
 *
 * @see App::session()
 * @see Framework\Session\Session::setOptions()
 * @see Framework\Session\SaveHandlers\DatabaseHandler::prepareConfig()
 * @see Framework\Session\SaveHandlers\FilesHandler::prepareConfig()
 * @see Framework\Session\SaveHandlers\MemcachedHandler::prepareConfig()
 * @see Framework\Session\SaveHandlers\RedisHandler::prepareConfig()
 * @see https://docs.aplus-framework.com/guides/libraries/mvc/index.html#session-service
 */

use Framework\Session\SaveHandlers\FilesHandler;

return [
    'default' => [
        'options' => [],
        'save_handler' => [
            'class' => FilesHandler::class,
            'config' => [
                'prefix' => '',
                'directory' => STORAGE_DIR . 'sessions',
                'match_ip' => false,
                'match_ua' => false,
            ],
        ],
        'logger_instance' => 'default',
        'auto_start' => true,
    ],
];
