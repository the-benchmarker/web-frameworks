<?php
//Include views
require_once 'view.php';

//URLs routing
$urlpatterns = [
  '/' => 'view.index',
  '/user' => 'view.user',
  '/user/{id}' => 'view.id',
];
