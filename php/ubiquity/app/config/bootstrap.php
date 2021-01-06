<?php
use Ubiquity\devtools\cmd\ConsoleFormatter as Console;

//Comments

//For development mode initialization
function _dev($devtools,$config){
		echo Console::showInfo("Development mode");
}

//For Production mode initialization
function _prod($devtools,$config){
	echo Console::showInfo("Production mode");
	$devtools->run('composer','optimize');
}

//Executed before all modes
function bs_before($devtools,$config){

}

//Executed after all modes
function bs_after($devtools,$config){
	//Initialize all caches
	$devtools->run('init-cache');
}