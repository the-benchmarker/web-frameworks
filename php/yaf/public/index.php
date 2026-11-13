<?php

$app = new Yaf_Application([
    'application' => [
        'directory' => dirname(__DIR__) . '/application',
    ],
]);

$app->bootstrap()->run();
