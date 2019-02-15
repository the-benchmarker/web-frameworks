<?php

ApplicationRoutes::draw(function() {
  get("/", "app#index");
  get("/user/:id", "user#get");
  post("/user", "user#create");
});

?>
