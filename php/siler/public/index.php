<?php declare(strict_types=1);

use function Siler\Functional\puts;
use function Siler\Route\get;
use function Siler\Route\post;

require_once __DIR__ . '/../vendor/autoload.php';

get('/', puts(''));

get('/user/{id}', function ($params) {
    echo $params['id'];
});

post('/user', puts(''));
