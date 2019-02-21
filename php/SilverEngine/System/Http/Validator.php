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

namespace Silver\Http;

class Validator
{
    private static $data = [];
    private static $errors = [];

    public static function check($data, $validators)
    {
        self::$data = $data;
        self::$errors = $errors = [];
        foreach($validators as $key => $validator) {
            $value = isset($data[$key]) ? $data[$key] : null;

            foreach(explode("|", $validator) as $valid) {
                $fun_args = explode(":", $valid);
                $fun_args[] = '';

                $fun = "check" . ucfirst($fun_args[0]);
                $ret = call_user_func_array("self::$fun", array_merge([$value], explode(',', $fun_args[1])));

                if($ret) {
                    $ret = str_replace('KEY', $key, $ret);
                    $errors[] = $ret;

                    if(!isset(self::$errors[$key])) {
                        self::$errors[$key] = [];
                    }
                    self::$errors[$key][] = $ret;
                }
            }
        }
        return $errors;
    }

    public static function get($key) 
    {
        if(isset(self::$errors[$key])) {
            return self::$errors[$key];
        }
        return [];
    }

    public static function pass() 
    {
        return !self::$errors;
    }

    private static function checkMin($value, $min) 
    {
        if(strlen($value) < $min) {
            return "KEY must have at least $min characters.";
        }
        return false;
    }

    private static function checkMax($value, $max) 
    {
        if(strlen($value) > $max) {
            return "KEY must have less than $max characters.";
        }
        return false;
    }

    private static function checkRequired($value) 
    {
        if(!$value) {
            return "KEY is required!";
        }
        return false;
    }

    private static function checkMatch($value, $key) 
    {
        if(!isset(self::$data[$key]) or $value != self::$data[$key]) {
            return "$key is not match  KEY";
        }
        return false;
    }

    private static function checkExist($value, $model) 
    {
        //TODO: check
        return false;
    }

    private static function checkUnique($value) 
    {
        // TODO
        return false;
    }
}
