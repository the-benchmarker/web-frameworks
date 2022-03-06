<?php

require_once __DIR__ . '/vendor/autoload.php';

use FrameworkX\App;
use Psr\Http\Message\ServerRequestInterface;
use React\Http\Message\Response;

$app = new App();

$app->get('/', fn (ServerRequestInterface $request) => Response::plaintext(''));

$app->post('/user', fn (ServerRequestInterface $request) => Response::plaintext(''));

$app->get('/user/{id}', fn (ServerRequestInterface $request) => Response::plaintext(
    $request->getAttribute('id', ''),
));

$app->run();
