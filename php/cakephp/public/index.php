<?php

require dirname(__DIR__) . '/vendor/autoload.php';

use App\Application;
use Cake\Http\Server;

$server = new Server(new Application(dirname(__DIR__) . '/config'));

$server->emit($server->run());
