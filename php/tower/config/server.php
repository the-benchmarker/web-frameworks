<?php

return [
    'listen' => 'http://127.0.0.1:9000',
    'transport' => 'tcp',
    'context' => [],
    'count' => cpuCoreCount(),
    'user' => '',
    'group' => '',
    'max_request' => 1000000,
    'max_package_size' => 10 * 1024 * 1024
];
