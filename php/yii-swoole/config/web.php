<?php

$config = [
    'id' => 'basic',
    'basePath' => dirname(__DIR__),
    'aliases' => [
        '@bower' => '@vendor/bower-asset',
        '@npm'   => '@vendor/npm-asset',
    ],
    'components' => [
        'request' => [
            'cookieValidationKey' => 'mBO2FH2t7yszxSq_zltjvhVefvfClqjN',
        ],
        'user' => [
            'enableSession' => false,
            'enableAutoLogin' => false,
        ],
        'urlManager' => [
            'enablePrettyUrl' => true,
            'enableStrictParsing' => true,
            'showScriptName' => false,
            'rules' => [
                '' => 'site/index',
                'user' => 'user/index',
                'user/<id:.+>' => 'user/search',
            ],
        ]
    ],
];

return $config;
