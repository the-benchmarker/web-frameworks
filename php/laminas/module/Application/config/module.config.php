<?php

namespace Application;

use Laminas\Router\Http\Literal;
use Laminas\Router\Http\Segment;
use Laminas\ServiceManager\Factory\InvokableFactory;

/**
 * Laminas Framework Module Configuration
 * 
 * Benchmark server routes following PHP best practices.
 */
return [
    'router' => [
        'routes' => [
            'home' => [
                'type' => Literal::class,
                'options' => [
                    'route'    => '/',
                    'defaults' => [
                        'controller' => Controller\IndexController::class,
                        'action'     => 'index',
                    ],
                ],
            ],
            'health' => [
                'type' => Literal::class,
                'options' => [
                    'route'    => '/health',
                    'defaults' => [
                        'controller' => Controller\IndexController::class,
                        'action'     => 'health',
                    ],
                ],
            ],
            'user_id' => [
                'type'    => Segment::class,
                'options' => [
                    'route'    => '/user[/:id]',
                    'defaults' => [
                        'controller' => Controller\IndexController::class,
                        'action'     => 'userId',
                    ],
                ],
            ],
            'user' => [
                'type' => Literal::class,
                'options' => [
                    'route' => '/user',
                    'defaults' => [
                        'controller' => Controller\IndexController::class,
                        'action' => 'user'
                    ]
                ]
            ]
        ],
    ],
    'controllers' => [
        'factories' => [
            Controller\IndexController::class => InvokableFactory::class,
        ],
    ],
];
