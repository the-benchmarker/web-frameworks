<?php

define('PHP_START', microtime(true));

use Cubex\Cubex;
use App\Application;

$loader = require_once(dirname(__DIR__) . '/vendor/autoload.php');
try {
    $cubex = new Cubex(dirname(__DIR__), $loader);
    $cubex->handle(new Application());
} catch(Throwable $e) {

} finally {
    if($cubex instanceof Cubex) {
        //Call the shutdown command
        $cubex->shutdown();
    }
}
