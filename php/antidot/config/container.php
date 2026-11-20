<?php

declare(strict_types=1);

// Load configuration
use Antidot\Container\Builder;

$config = require __DIR__ . '/../config/config.php';

// Build container
return Builder::build($config, true);
