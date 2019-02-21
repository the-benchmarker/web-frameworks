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

namespace App;

use Silver\Core\Route;
use Silver\Core\Env;

if (Env::name() == 'local') {
    //create migration
    Route::get('/migrate/{modelName?}', 'Migrations@up', 'migrate');
    //drop migrations
    Route::get('/migrate-down/{modelName?}', 'Migrations@down', 'migrate');
    //run migrations and seeds
    Route::get('/migrate-seed', 'Migrations@all', 'migrate');
}
