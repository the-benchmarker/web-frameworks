<?php

declare(strict_types=1);

use Spiral\Stempler\Builder;
use Spiral\Stempler\Directive;
use Spiral\Stempler\Transform\Finalizer;
use Spiral\Stempler\Transform\Visitor;
use Spiral\Views\Processor;

return [
    'directives' => [
        // available Blade-style directives
        Directive\PHPDirective::class,
        Directive\RouteDirective::class,
        Directive\LoopDirective::class,
        Directive\JsonDirective::class,
        Directive\ConditionalDirective::class,
        Directive\ContainerDirective::class,
    ],
    'processors' => [
        // cache depended source processors (i.e. LocaleProcessor)
        Processor\ContextProcessor::class,
    ],
    'visitors' => [
        Builder::STAGE_PREPARE => [
            // visitors to be invoked before transformations
            Visitor\DefineBlocks::class,
            Visitor\DefineAttributes::class,
            Visitor\DefineHidden::class,
        ],
        Builder::STAGE_TRANSFORM => [
            // visitors to be invoked during transformations
        ],
        Builder::STAGE_FINALIZE => [
            // visitors to be invoked on after the transformations is over
            Visitor\DefineStacks::class,
            Finalizer\StackCollector::class,
        ],
        Builder::STAGE_COMPILE => [
            // visitors to be invoked on compilation stage
        ],
    ],
];
