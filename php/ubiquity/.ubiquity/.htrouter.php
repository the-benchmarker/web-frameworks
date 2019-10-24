<?php
$uri = ltrim(urldecode(parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH)),'/');
if ($uri==null || !file_exists(__DIR__ . '/../' .$uri)) {
	$_GET['c'] = $uri;
	include_once '_index.php';
	return true;
}else{
	$_GET['c']='';
	return false;
}
