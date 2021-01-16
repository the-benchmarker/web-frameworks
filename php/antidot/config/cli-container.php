<?php

// Load configuration
use Antidot\Container\Builder;

$config = require __DIR__ . '/../config/cli-config.php';

return Builder::build($config, true);
