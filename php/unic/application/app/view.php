<?php
//Include models
require_once 'model.php';

//Create your views here
class view extends Views {
  function home(Request $req) {
    return $this->response('');
  }

  function user(Request $req) {
    return $this->response('');
  }

  function create(Request $req) {
    return $this->response($req->params->id);
  }

  function page_not_found(Request $req) {
    $this->response_code(404);
    return $this->render('errors/404');
  }

  function forbidden(Request $req) {
    $this->response_code(403);
    return $this->render('errors/403');
  }

  function internal_server_error(Request $req) {
    $this->response_code(500);
    return $this->render('errors/500');
  }
}
