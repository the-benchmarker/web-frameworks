<?php

/**
 * SilverEngine  - PHP MVC framework
 *
 * @package   SilverEngine
 * @author    SilverEngine Team
 * @copyright 2015-2017
 * @license   MIT
 * @link      https://github.com/SilverEngine/Framework
 */


return [
    'databases' => [
        'on' => false,
        'default' => 'local',
        'local'   => [
            'service'       => false,
            'driver'        => 'mysql',
            'hostname'      => 'localhost',
            'username'      => 'root',
            'password'      => '',
            'basename'      => '',
            'limit_request' => 25,
        ]
    ]
];