<?php
/*
 * This file is part of App Project.
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
/**
 * Mailer config.
 *
 * @see App::mailer()
 * @see Framework\Email\Mailer::makeConfig()
 * @see https://docs.aplus-framework.com/guides/libraries/mvc/index.html#mailer-service
 */

use Framework\Email\Mailers\SMTPMailer;

return [
    'default' => [
        'class' => SMTPMailer::class,
        'config' => [
            'host' => 'localhost',
            'port' => 587,
            'tls' => true,
            'username' => null,
            'password' => null,
            'charset' => 'utf-8',
            'crlf' => "\r\n",
            'keep_alive' => false,
            'add_logs' => true,
        ],
    ],
];
