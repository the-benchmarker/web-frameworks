<?php

Route::get('/', '');
Route::get('/user/{id}', preview('{%id%}'));
Route::post('/user', '');
