<?php
/*
 * This file is part of App Project.
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
/**
 * Router config.
 *
 * @see App::router()
 * @see https://docs.aplus-framework.com/guides/libraries/mvc/index.html#router-service
 */
return [
    'default' => [
        'files' => [
            BOOT_DIR . 'routes.php',
        ],
        'auto_options' => false,
        'auto_methods' => false,
        'placeholders' => [],
        'response_instance' => 'default',
        'language_instance' => 'default',
    ],
];
