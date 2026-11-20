<?php
/*
 * This file is part of App Project.
 *
 * For the full copyright and license information, please view the LICENSE
 * file that was distributed with this source code.
 */
/**
 * Cache config.
 *
 * @see App::cache()
 * @see Framework\Cache\FilesCache::$configs
 * @see Framework\Cache\MemcachedCache::$configs
 * @see Framework\Cache\RedisCache::$configs
 * @see https://docs.aplus-framework.com/guides/libraries/mvc/index.html#cache-service
 */

use Framework\Cache\FilesCache;
use Framework\Cache\Serializer;

return [
    'default' => [
        'class' => FilesCache::class,
        'configs' => [
            'directory' => STORAGE_DIR . 'cache',
        ],
        'prefix' => null,
        'serializer' => Serializer::PHP,
        'logger_instance' => 'default',
    ],
];
