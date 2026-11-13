<?php

use Cake\Core\Configure;

require __DIR__ . DIRECTORY_SEPARATOR . 'paths.php';
require CORE_PATH . 'config' . DS . 'bootstrap.php';

Configure::write(require CONFIG . 'app.php');

date_default_timezone_set(Configure::read('App.defaultTimezone'));
mb_internal_encoding(Configure::read('App.encoding'));
ini_set('intl.default_locale', Configure::read('App.defaultLocale'));
