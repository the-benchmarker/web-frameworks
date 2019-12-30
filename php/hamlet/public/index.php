<?php

require_once __DIR__ . '/../vendor/autoload.php';

use Application\Application;
use Hamlet\Http\Bootstraps\ServerBootstrap;

$application = new Application();
ServerBootstrap::run($application);
