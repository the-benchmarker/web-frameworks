<?php
class UserController extends ApplicationController {
  public function get($id) {
    $this->render(["text" => $id]);
  }
  public function create() {
    $this->render(["text" => ""]);
  }
}
