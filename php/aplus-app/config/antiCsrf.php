<?php
/*
 * This file is part of App Project.
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
/**
 * Anti-CSRF config.
 *
 * @see App::antiCsrf()
 * @see https://docs.aplus-framework.com/guides/libraries/mvc/index.html#anti-csrf-service
 */
return [
    'default' => [
        'enabled' => true,
        'token_name' => 'csrf_token',
        'request_instance' => 'default',
        'session_instance' => 'default',
    ],
];
