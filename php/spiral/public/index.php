<?php

declare(strict_types=1);

use App\Application\Kernel;
use App\Application\Exception\Handler;

// If you forgot to configure some of this in your php.ini file,
// then don't worry, we will set the standard environment
// settings for you.

\mb_internal_encoding('UTF-8');
\error_reporting(E_ALL | E_STRICT ^ E_DEPRECATED);
\ini_set('display_errors', 'stderr');

// Register Composer's auto loader.
require __DIR__ . '/vendor/autoload.php';


// Initialize shared container, bindings, directories and etc.
$app = Kernel::create(
    directories: ['root' => __DIR__],
    exceptionHandler: Handler::class,
)->run();

if ($app === null) {
    exit(255);
}

$code = (int)$app->serve();
exit($code);
