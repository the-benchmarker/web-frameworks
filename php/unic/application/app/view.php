<?php
//Include models
require_once 'model.php';

//Create your views here
class view extends Views {
  function home(Request $req) {
    return $this->response('');
  }

  function create(Request $req) {
    return $this->response('');
  }

  function user(Request $req) {
    return $this->response($req->params->id);
  }
}
