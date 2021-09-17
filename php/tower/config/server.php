<?php

return [
    'listen' => 'http://0.0.0.0:3000',
    'transport' => 'tcp',
    'context' => [],
    'count' => cpuCoreCount() * 2,
    'user' => '',
    'group' => '',
    'max_request' => 1000000,
    'max_package_size' => 10 * 1024 * 1024
];
