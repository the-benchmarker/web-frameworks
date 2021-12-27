<?php
//Include views
require_once 'view.php';

//URLs routing
$urlpatterns = [
  '/' => 'view.home',
  '/user' => 'view.create_user',
  '/user/{id}' => 'view.get_user',
];
