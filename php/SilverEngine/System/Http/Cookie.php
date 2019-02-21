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

use Silver\Core\Env;
use Silver\Support\Crypter;

/**
 * Cookie
 */
class Cookie
{

    private static $name;
    private static $value;
    private static $expire = 10 * 365 * 24 * 60 * 60; //Default cookie expire is 10 years

    /**
     * @param     $name
     * @param     $value
     * @param int     $expire
     * @param     $secured
     *
     * @return Cookie
     */
    public static function set($name, $value, $expire = 0, $extend = false)
    {
        //need to add check for global variable for encryption

        self::$name = $name;

        if ($expire > 0 ) {
            self::$expire = $expire;
        }
        if ($extend == true ) {
            $time = self::$expire;
        } else {
            $time = time() + self::$expire;
        }

        //First we check if data $value is array or string
        if (is_array($value) ) {
            $data = [
                "values"    => $value,
                "expire"    => $time,
                "encrypted" => '0'
            ];
            self::setCookie($name, json_encode($data), time() + self::$expire);
            $data["encrypted"] = 1;
            self::$value = json_encode($data);
        } else {
            $data = [
                "values"    => array($value),
                "expire"    => $time,
                "encrypted" => '0'
            ];
            self::setCookie($name, json_encode($data), time() + self::$expire);
            $data["encrypted"] = 1;
            self::$value = json_encode($data);
        }

        return new self;
    }


    public function encrypt()
    {
        if (isset($_COOKIE) ) {
            $name = self::$name;
            $value = Crypter::crypt(self::$value);
            self::setCookie($name, $value, time() + self::$expire);

            return true;
        } else {
            return false;
        }
    }


    /**
     * @param      $name
     * @param null $return_type null=values, expire=expire, all=all
     *
     * @return mixed
     */
    public static function get($name, $return_type = null)
    {
        //Difrend typed of return...meybe one function for that
        if (self::exists($name) ) {
            self::$name = $name;
            if (self::is_crypted($name) == true ) {
                $ret = json_decode(Crypter::decrypt($_COOKIE[$name]), true);
            } else {
                $ret = json_decode($_COOKIE[$name], true);
            }
            //Return type 0 return only array of values
            if ($return_type == null ) {
                return $ret['values'];

            } else if ($return_type == "expire" ) {
                return $ret['expire'];

            } else if ($return_type == "all" ) {
                return $ret;
            }else{
                return $ret['values'];
            }
        }
    }

    /**
     * @param $value
     *
     * @return bool|Cookie
     */
    public static function is_crypted($value)
    {
        if (! is_null($value) ) {
            if (Crypter::decrypt($_COOKIE[$value]) != "" ) {
                //Cokie is crypted
                return true;
            } else {
                //Cokie is not crypted
                return false;
            }
        }

        return new self;
    }



    //This function will attach new value to existant cookie
    /**
     * @param $key
     * @param $new_value
     *
     * @return bool
     */
    public static function attach($key, $new_value)
    {
        $old_data = Cookie::get($key);
        $old_values = $old_data['values'];
        $old_expire = $old_data['expire'];
        $old_encrypted = $old_data['encrypted'];

        if (is_array($old_data) ) {
            if (! is_array($new_value) ) {
                $new_value = array($new_value);
            }
            if ($old_encrypted == 1 ) {
                Cookie::set($key, array_merge($old_values, $new_value), $old_expire, true)->encrypt();
            } else {
                Cookie::set($key, array_merge($old_values, $new_value), $old_expire, true);
            }

            return true;
        } else {
            return false;
        }
    }


    /**
     * @param $key
     * @param $value_to_delete
     *
     * @return bool
     */
    public static function detach($key, $value_to_delete) //Key or index
    {
        $old_data = Cookie::get($key);
        $old_values = $old_data['values'];
        $old_expire = $old_data['expire'];
        $old_encrypted = $old_data['encrypted'];
        if (is_array($old_data) ) {
            unset($old_values[$value_to_delete]);
            if ($old_encrypted == 1 ) {
                Cookie::set($key, $old_values, $old_expire, true)->encrypt();
            } else {
                Cookie::set($key, $old_values, $old_expire, true);
            }

            return true;
        } else {
            return false;
        }
    }


    public static function all()
    {
        return $_COOKIE;
    }


    public static function exists($key)
    {
        return isset(self::all()[$key]);
    }


    public static function delete($key)
    {
        self::set($key, '', - 1);
    }

    public static function flush()
    {
        foreach ( self::all() as $key => $value ) {
            self::delete($key);
        }
    }

    private static function setCookie($key, $value, $expiration) 
    {
        Response::instance()->setCookie($key, $value, $expiration);
    }
}
