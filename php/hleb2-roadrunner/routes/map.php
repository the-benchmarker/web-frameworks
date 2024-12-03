<?php

Route::get('/', preview(''));
Route::get('/user/{id}', preview('{%id%}'));
Route::post('/user', preview(''));
