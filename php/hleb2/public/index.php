<?php

use Hleb\HlebBootstrap;

define('HLEB_PUBLIC_DIR', realpath(__DIR__));
define('HLEB_GLOBAL_DIR', realpath(__DIR__ . '/../'));

require HLEB_GLOBAL_DIR . '/vendor/phphleb/framework/HlebBootstrap.php';

(new HlebBootstrap(HLEB_PUBLIC_DIR))->load();
