<?php
/*
 * This file is part of App Project.
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
/**
 * View config.
 *
 * @see App::view()
 * @see https://docs.aplus-framework.com/guides/libraries/mvc/index.html#view-service
 */
return [
    'default' => [
        'base_dir' => APP_DIR . 'Views',
        'extension' => '.php',
        'layout_prefix' => '_layouts',
        'include_prefix' => '_includes',
    ],
];
