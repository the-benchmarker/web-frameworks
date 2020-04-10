<?php

use Siler\Route;
use Siler\Functional as λ;

chdir(dirname(dirname(__DIR__)));
require __DIR__ . '/../vendor/autoload.php';

Route\get('/', λ\puts(''));

Route\get('/user/{id}', function ($params) {
    printf('%s', $params['id']);
});

Route\post('/user', λ\puts(''));
