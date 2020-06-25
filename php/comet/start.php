<?php
require_once __DIR__ . '/vendor/autoload.php';

use Comet\Comet;

$app = new Comet([ 
	'port' => 3000,
	shell_exec('nproc') ?? 32,
]);


$app->get('/', function ($request, $response) {
    return $response;
});

$app->post('/user', function ($request, $response) {
    return $response;
});

$app->get('/user/{id}', function ($request, $response, $args) {
    return $response
    	->with($args['id']);
});

$app->run();
