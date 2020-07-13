<?php namespace Config;

use CodeIgniter\Config\BaseConfig;

class Toolbar extends BaseConfig
{
	/*
	|--------------------------------------------------------------------------
	| Debug Toolbar
	|--------------------------------------------------------------------------
	| The Debug Toolbar provides a way to see information about the performance
	| and state of your application during that page display. By default it will
	| NOT be displayed under production environments, and will only display if
	| CI_DEBUG is true, since if it's not, there's not much to display anyway.
	|
	| toolbarMaxHistory = Number of history files, 0 for none or -1 for unlimited
	|
	*/
	public $collectors = [
		\CodeIgniter\Debug\Toolbar\Collectors\Timers::class,
		\CodeIgniter\Debug\Toolbar\Collectors\Database::class,
		\CodeIgniter\Debug\Toolbar\Collectors\Logs::class,
		\CodeIgniter\Debug\Toolbar\Collectors\Views::class,
		// \CodeIgniter\Debug\Toolbar\Collectors\Cache::class,
		\CodeIgniter\Debug\Toolbar\Collectors\Files::class,
		\CodeIgniter\Debug\Toolbar\Collectors\Routes::class,
		\CodeIgniter\Debug\Toolbar\Collectors\Events::class,
	];

	/*
	|--------------------------------------------------------------------------
	| Max History
	|--------------------------------------------------------------------------
	| The Toolbar allows you to view recent requests that have been made to
	| the application while the toolbar is active. This allows you to quickly
	| view and compare multiple requests.
	|
	| $maxHistory sets a limit on the number of past requests that are stored,
	| helping to conserve file space used to store them. You can set it to
	| 0 (zero) to not have any history stored, or -1 for unlimited history.
	|
	*/
	public $maxHistory = 20;

	/*
	|--------------------------------------------------------------------------
	| Toolbar Views Path
	|--------------------------------------------------------------------------
	| The full path to the the views that are used by the toolbar.
	| MUST have a trailing slash.
	|
	*/
	public $viewsPath = SYSTEMPATH . 'Debug/Toolbar/Views/';

	/*
	|--------------------------------------------------------------------------
	| Max Queries
	|--------------------------------------------------------------------------
	| If the Database Collector is enabled, it will log every query that the
	| the system generates so they can be displayed on the toolbar's timeline
	| and in the query log. This can lead to memory issues in some instances
	| with hundreds of queries.
	|
	| $maxQueries defines the maximum amount of queries that will be stored.
	|
	*/
	public $maxQueries = 100;
}
