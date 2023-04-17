<?php

require_once __DIR__ . '/vendor/autoload.php';

use FrameworkX\App;
use Psr\Http\Message\ServerRequestInterface;
use React\Http\Message\Response;

$handlerOK = fn (ServerRequestInterface $request) => Response::plaintext('');
$handlerID = fn (ServerRequestInterface $request) => Response::plaintext($request->getAttribute('id'));

$app = new App();

$app->get('/', $handlerOK);
$app->post('/user', $handlerOK);
$app->get('/user/{id}', $handlerID);

$app->run();
