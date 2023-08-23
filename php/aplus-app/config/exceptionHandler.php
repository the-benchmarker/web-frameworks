<?php
/*
 * This file is part of App Project.
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
/**
 * Exceptions config.
 *
 * @see App::exceptionHandler()
 * @see https://docs.aplus-framework.com/guides/libraries/mvc/index.html#exception-handler-service
 */

use Framework\Debug\ExceptionHandler;

return [
    'default' => [
        'initialize' => true,
        'environment' => IS_DEV
            ? ExceptionHandler::DEVELOPMENT
            : ExceptionHandler::PRODUCTION,
        'development_view' => null,
        'production_view' => null,
        'logger_instance' => 'default',
        'language_instance' => 'default',
    ],
];
