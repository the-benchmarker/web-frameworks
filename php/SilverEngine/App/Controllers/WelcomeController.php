<?php

namespace App\Controllers;

use Silver\Core\Controller;
use Silver\Http\View;
use Silver\Http\Request;

class WelcomeController extends Controller
{

     //Without template Engine

    public function welcome()
    {
        return "test";
    }

    public function getId($id = false)
    {
        return $id;
    }

    public function post(Request $req)
    {
        return "post works";
    }

    //With template Engine

    public function testMeWithTE($id = false)
    {
        if($id){
            return View::make('get')->with('user', $id);
        }
        else{
            return View::make('all');
        }
    }

    public function testPostWithTE()
    {
        return View::make('post');
    }
}
