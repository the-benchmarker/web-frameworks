<?php

declare(strict_types=1);

use Cycle\ORM\Collection\ArrayCollectionFactory;
use Cycle\ORM\Collection\DoctrineCollectionFactory;
use Cycle\ORM\Collection\IlluminateCollectionFactory;
use Cycle\ORM\Collection\LoophpCollectionFactory;

return [
    'schema' => [
        /**
         * true (Default) - Schema will be stored in a cache after compilation.
         * It won't be changed after entity modification. Use `php app.php cycle` to update schema.
         *
         * false - Schema won't be stored in a cache after compilation.
         * It will be automatically changed after entity modification. (Development mode)
         */
        'cache' => env('CYCLE_SCHEMA_CACHE', true),

        /**
         * The CycleORM provides the ability to manage default settings for
         * every schema with not defined segments
         */
        'defaults' => [
            // SchemaInterface::MAPPER => \Cycle\ORM\Mapper\Mapper::class,
            // SchemaInterface::REPOSITORY => \Cycle\ORM\Select\Repository::class,
            // SchemaInterface::SCOPE => null,
            // SchemaInterface::TYPECAST_HANDLER => [
            //    \Cycle\ORM\Parser\Typecast::class, \App\Infrastructure\CycleORM\Typecaster\UuidTypecast::class,
            // ],
        ],

        'collections' => [
            'default' => 'doctrine',
            'factories' => ['doctrine' => new DoctrineCollectionFactory()],
        ],

        /**
         * Schema generators (Optional)
         * null (default) - Will be used schema generators defined in bootloaders
         */
        'generators' => null,

        // 'generators' => [
        //        \Cycle\Annotated\Embeddings::class,
        //        \Cycle\Annotated\Entities::class,
        //        \Cycle\Annotated\MergeColumns::class,
        //        \Cycle\Schema\Generator\ResetTables::class,
        //        \Cycle\Schema\Generator\GenerateRelations::class,
        //        \Cycle\Schema\Generator\ValidateEntities::class,
        //        \Cycle\Schema\Generator\RenderTables::class,
        //        \Cycle\Schema\Generator\RenderRelations::class,
        //        \Cycle\Annotated\TableInheritance::class,
        //        \Cycle\Annotated\MergeIndexes::class
        //        \Cycle\Schema\Generator\GenerateTypecast::class,
        // ],
    ],

    'warmup' => env('CYCLE_SCHEMA_WARMUP', false),

    /**
     * Custom relation types for entities
     */
    'customRelations' => [
        // \Cycle\ORM\Relation::EMBEDDED => [
        //     \Cycle\ORM\Config\RelationConfig::LOADER => \Cycle\ORM\Select\Loader\EmbeddedLoader::class,
        //     \Cycle\ORM\Config\RelationConfig::RELATION => \Cycle\ORM\Relation\Embedded::class,
        // ]
    ],
];
