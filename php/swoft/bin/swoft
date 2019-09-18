#!/usr/bin/env php
<?php declare(strict_types=1);

// Bootstrap
require_once __DIR__ . '/bootstrap.php';

Swoole\Coroutine::set([
    'max_coroutine' => 300000,
]);

// Run application
(new \App\Application())->run();
