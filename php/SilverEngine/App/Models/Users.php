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

namespace App\Models;

use Silver\Database\Model;

/**
 * Users Model
 */
class Users extends Model
{
    protected static $_table = 'users';

    protected $hidden = [
        'password',
        'salt',
        'delete_at',
    ];
}
