<?php
/*
 * This file is part of App Project.
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
/**
 * Autoloader config.
 *
 * @see App::autoloader()
 * @see https://docs.aplus-framework.com/guides/libraries/mvc/index.html#autoloader-service
 */
return [
    'default' => [
        'register' => true,
        'extensions' => '.php',
        'namespaces' => [
            'App' => APP_DIR,
        ],
        'classes' => [],
    ],
];
