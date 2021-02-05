<?php
//Include views
require_once 'view.php';

//URLs routing
$urlpatterns = [
  '/' => 'view.home',
  '/user' => 'view.user',
  '/user/{id}' => 'view.create',
];
