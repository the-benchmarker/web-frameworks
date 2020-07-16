<?php namespace Config;

use CodeIgniter\Config\BaseConfig;

class Cache extends BaseConfig
{
	/*
	|--------------------------------------------------------------------------
	| Primary Handler
	|--------------------------------------------------------------------------
	|
	| The name of the preferred handler that should be used. If for some reason
	| it is not available, the $backupHandler will be used in its place.
	|
	*/
	public $handler = 'file';

	/*
	|--------------------------------------------------------------------------
	| Backup Handler
	|--------------------------------------------------------------------------
	|
	| The name of the handler that will be used in case the first one is
	| unreachable. Often, 'file' is used here since the filesystem is
	| always available, though that's not always practical for the app.
	|
	*/
	public $backupHandler = 'dummy';

	/*
	|--------------------------------------------------------------------------
	| Cache Directory Path
	|--------------------------------------------------------------------------
	|
	| The path to where cache files should be stored, if using a file-based
	| system.
	|
	*/
	public $storePath = WRITEPATH . 'cache/';

	/*
	|--------------------------------------------------------------------------
	| Cache Include Query String
	|--------------------------------------------------------------------------
	|
	| Whether to take the URL query string into consideration when generating
	| output cache files. Valid options are:
	|
	|	false      = Disabled
	|	true       = Enabled, take all query parameters into account.
	|	             Please be aware that this may result in numerous cache
	|	             files generated for the same page over and over again.
	|	array('q') = Enabled, but only take into account the specified list
	|	             of query parameters.
	|
	*/
	public $cacheQueryString = false;

	/*
	|--------------------------------------------------------------------------
	| Key Prefix
	|--------------------------------------------------------------------------
	|
	| This string is added to all cache item names to help avoid collisions
	| if you run multiple applications with the same cache engine.
	|
	*/
	public $prefix = '';

	/*
	| -------------------------------------------------------------------------
	| Memcached settings
	| -------------------------------------------------------------------------
	| Your Memcached servers can be specified below, if you are using
	| the Memcached drivers.
	|
	|	See: https://codeigniter.com/user_guide/libraries/caching.html#memcached
	|
	*/
	public $memcached = [
		'host'   => '127.0.0.1',
		'port'   => 11211,
		'weight' => 1,
		'raw'    => false,
	];

	/*
	| -------------------------------------------------------------------------
	| Redis settings
	| -------------------------------------------------------------------------
	| Your Redis server can be specified below, if you are using
	| the Redis or Predis drivers.
	|
	*/
	public $redis = [
		'host'     => '127.0.0.1',
		'password' => null,
		'port'     => 6379,
		'timeout'  => 0,
		'database' => 0,
	];

	/*
	|--------------------------------------------------------------------------
	| Available Cache Handlers
	|--------------------------------------------------------------------------
	|
	| This is an array of cache engine alias' and class names. Only engines
	| that are listed here are allowed to be used.
	|
	*/
	public $validHandlers = [
		'dummy'     => \CodeIgniter\Cache\Handlers\DummyHandler::class,
		'file'      => \CodeIgniter\Cache\Handlers\FileHandler::class,
		'memcached' => \CodeIgniter\Cache\Handlers\MemcachedHandler::class,
		'predis'    => \CodeIgniter\Cache\Handlers\PredisHandler::class,
		'redis'     => \CodeIgniter\Cache\Handlers\RedisHandler::class,
		'wincache'  => \CodeIgniter\Cache\Handlers\WincacheHandler::class,
	];
}
