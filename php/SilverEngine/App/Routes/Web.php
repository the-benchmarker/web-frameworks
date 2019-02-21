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

namespace App\Routes;

use Silver\Core\Route;

Route::get('/', 'Welcome@welcome', 'welcome', 'public');
Route::get('/user/{id?}', 'Welcome@getId', 'getId', 'public');
Route::post('/user', 'Welcome@post', 'post', 'public');
//Route::get('/', 'Welcome@testMeWithTE', 'testMeWithTE', 'public');
//Route::get('/user', 'Welcome@testPostWithTE', 'testPostWithTE', 'public');

