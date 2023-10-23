<?php
/*
 * This file is part of App Project.
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
if (isset($_SERVER['ENVIRONMENT']) && $_SERVER['ENVIRONMENT'] === 'development') {
    error_reporting(-1);
    ini_set('display_errors', 'On');
} else {
    error_reporting(\E_ALL & ~\E_DEPRECATED & ~\E_NOTICE & ~\E_STRICT & ~\E_USER_DEPRECATED & ~\E_USER_NOTICE);
    ini_set('display_errors', 'Off');
}

date_default_timezone_set('UTC');
//set_time_limit(30);
