<?php

//Create your views here
class view extends Views
{
    function index(Request $req)
    {
        $this->response('');
    }

    function user(Request $req)
    {
        $this->response('');
    }

    function id(Request $req)
    {
        $this->response($req->params->id);
    }
}
