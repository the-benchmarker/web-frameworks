<?php

namespace App\Controller;

use Cake\Controller\Controller;
use Cake\Http\Response;

class UserController extends Controller
{
    public function view(string $id): Response
    {
        return $this->response->withStringBody($id);
    }

    public function add(): Response
    {
        return $this->response;
    }
}
