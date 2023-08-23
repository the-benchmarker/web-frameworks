<?php
/*
 * This file is part of App Project.
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
/**
 * Validation config.
 *
 * @see App::validation()
 * @see https://docs.aplus-framework.com/guides/libraries/mvc/index.html#validation-service
 */

use Framework\MVC\Validator;
use Framework\Validation\FilesValidator;

return [
    'default' => [
        'validators' => [
            Validator::class,
            FilesValidator::class,
        ],
        'language_instance' => 'default',
    ],
];
