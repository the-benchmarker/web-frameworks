<?php

class default_c extends Controller
{
    use HOOK_D;

    function head_y($action) {
    }

    function tail_y() {
    }

    function empty_a() {
        return true;
    }

    function a_user() {
        return true;
    }

    function a_id($id) {
        echo $id;
    }
}
