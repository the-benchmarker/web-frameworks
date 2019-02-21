<?php

/**
 * SilverEngine  - PHP MVC framework
 *
 * @package   SilverEngine
 * @author    SilverEngine Team
 * @copyright 2015-2017
 * @license   MIT
 * @link      https://github.com/SilverEngine/Framework
 */

namespace Silver\Support;

/**
 * 
 */
class Fake
{
    private $arg;
    
    public static function name($arg)
    {
        if($arg == true) {
            echo "Max";
        } else {
            return 'Max';
        }
    }

    public static function username($arg)
    {
        if($arg == true) {
            echo "SilverEngine";
        } else {
            return 'SilverEngine';
        }
    }

    public static function id($arg)
    {
        if($arg == true) {
            echo rand(0, 100);
        } else {
            return rand(0, 100);
        }
    }

    public static function email($arg)
    {
        if($arg == true) {
            echo 'admin@localhost';
        } else {
            return 'admin@localhost';
        }
    }

    public static function address($arg)
    {
        if($arg == true) {
            echo 'Silver street 16';
        } else {
            return 'Silver street 16';
        }
    }

    public static function fulladdress($arg)
    {
        if($arg == true) {
            echo 'Silver street 16, 543669 New City';
        } else {
            return 'Silver street 16, 543669 New City';
        }
    }

    public static function number($arg)
    {
        if($arg == true) {
            echo rand(0, 100);
        } else {
            return rand(0, 100);
        }
    }

    public static function phonenumber($arg)
    {
        if($arg == true) {
            echo '+00 000 000 000';
        } else {
            return '+00 000 000 000';
        }
    }

    public static function title($arg)
    {
        if($arg == true) {
            echo 'My new title placeholder';
        } else {
            return 'My new title placeholder';
        }
    }

    public static function text($arg)
    {
        if($arg == true) {
            echo 'This is sample of text placeholder for your page. Thank you to choose Silver engine framework. This is Dynamical Model View Controller (DMVC) with build-in terminal engine to help you with quick and easy programing. For all info you can contact our Support team or send email on enginesilver@gmail.com';
        } else {
            return 'This is sample of text placeholder for your page. Thank you to choose Silver engine framework. This is Dynamical Model View Controller (DMVC) with build-in terminal engine to help you with quick and easy programing. For all info you can contact our Support team or send email on enginesilver@gmail.com';
        }
    }

    public static function image($arg)
    {
        if($arg == true) {
            echo '<img src="System/Libs/images/silverlogo.png">';
        } else {
            return '<img src="System/Libs/images/silverlogo.png">';
        }
    }

}