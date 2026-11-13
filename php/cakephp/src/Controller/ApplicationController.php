<?php

namespace App\Controller;

use Cake\Controller\Controller;
use Cake\Http\Response;

class ApplicationController extends Controller
{
    public function index(): Response
    {
        return $this->response;
    }
}
