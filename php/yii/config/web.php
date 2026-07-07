<?php

/**
 * Yii2 Framework Configuration
 * 
 * Benchmark server configuration following PHP best practices.
 */

$config = [
    'id' => 'benchmark-yii',
    'basePath' => dirname(__DIR__),
    'aliases' => [
        '@bower' => '@vendor/bower-asset',
        '@npm'   => '@vendor/npm-asset',
    ],
    'timeZone' => 'UTC',
    'components' => [
        'request' => [
            'cookieValidationKey' => 'mBO2FH2t7yszxSq_zltjvhVefvfClqjN',
            'enableCsrfValidation' => false, // Disable CSRF for benchmarking
            'parsers' => [
                'application/json' => 'yii\web\JsonParser',
            ],
        ],
        'user' => [
            'enableSession' => false,
            'enableAutoLogin' => false,
        ],
        'log' => [
            'traceLevel' => YII_DEBUG ? 3 : 0,
            'targets' => [
                [
                    'class' => 'yii\log\FileTarget',
                    'levels' => ['error', 'warning'],
                    'logVars' => [],
                ],
            ],
        ],
        'errorHandler' => [
            'errorAction' => 'site/error',
        ],
        'urlManager' => [
            'enablePrettyUrl' => true,
            'enableStrictParsing' => true,
            'showScriptName' => false,
            'rules' => [
                '' => 'site/index',
                'health' => 'site/health',
                'user' => 'user/index',
                'user/<id:.+>' => 'user/search',
            ],
        ],
        'response' => [
            'format' => yii\web\Response::FORMAT_RAW,
            'charset' => 'UTF-8',
        ],
    ],
    'params' => [
        'maxBodySize' => 16 * 1024 * 1024, // 16 MB
    ],
];

// Configure for production/benchmarking environment
if (!YII_DEBUG) {
    $config['components']['log']['targets'] = [
        [
            'class' => 'yii\log\FileTarget',
            'levels' => ['error', 'warning'],
            'logVars' => [],
        ],
    ];
}

return $config;
