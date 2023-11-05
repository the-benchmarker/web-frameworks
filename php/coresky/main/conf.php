<?php

define('START_TS', microtime(true));
define('DEV', false);
define('DEBUG', DEV ? 1 : 0); # 1, 2, 3 - verbosity level, 0 - debugging off
define('_PUBLIC', 'benchmarker.coresky');
define('PASS_CRYPT', 0);
define('PHP_TZ', 'Europe/Kiev');
define('DEFAULT_LG', '');
define('EXTRA', 0);     // 0 - off, 1 - on

ini_set('log_errors', 0);               # 0
ini_set('display_errors', DEV ? 1 : 0); # 1

define('DIR_M', basename(__DIR__));
define('DIR_S', is_dir('../air') ? '../air' : 'vendor/energy/air');
require DIR_S . '/sky.php';
SKY::$databases = ['driver' => 'sqlite3', 'dsn' => 'hole.base'];
