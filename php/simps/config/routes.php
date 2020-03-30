<?php

declare(strict_types=1);
/**
 * This file is part of Simps.
 *
 * @link     https://simps.io
 * @document https://doc.simps.io
 * @license  https://github.com/simple-swoole/simps/blob/master/LICENSE
 */

return [
    ['GET', '/', '\App\Controller\IndexController@index'],
    ['GET', '/user/{id:\d+}', '\App\Controller\IndexController@get'],
    ['POST', '/user', '\App\Controller\IndexController@create'],
];
