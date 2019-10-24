<?php
use Ubiquity\devtools\cmd\ConsoleFormatter as Console;
use Ubiquity\controllers\Startup;
use Ubiquity\cache\CacheManager;
use Ubiquity\controllers\Router;

//Comments

//For development mode initialization
function _dev($devtools,$config){
		echo Console::showInfo("Development mode");
}

//For Production mode initialization
//Create and store dynamic routes.
function _prod($devtools,$config){
	echo Console::showInfo("Production mode for benchmark");
	
	$devtools->run('composer','update');
	
	CacheManager::start($config);
	CacheManager::clearCache($config);
	
	Router::get('_default',function(){ echo '';});
	
	Router::get('/user/{id}', function($id){echo $id;});
	
	Router::post('/user',function(){echo '';});
	
	Router::addCallableRoute('{page}',function($page){\http_response_code(404);});
	CacheManager::storeDynamicRoutes(false);
	$devtools->run('composer','optimize');
	echo Console::showInfo("Dynamic routes created!");
}

//Executed before all modes
function bs_before($devtools,$config){

}

//Executed after all modes
function bs_after($devtools,$config){
	//Initialize all caches
	//$devtools->run('init-cache');
}
