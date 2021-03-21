<?php

return [
    'servers' => [
        'name' => 'http://0.0.0.0:3000',
        'count' => shell_exec('nproc')
    ]
];
