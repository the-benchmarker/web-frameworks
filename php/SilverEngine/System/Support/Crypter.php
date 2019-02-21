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
use Silver\Core\Env;

class Crypter
{

    /**
     * @param      $string
     * @param null   $password
     * @param null   $chiper
     * @return string
     */
    public static function crypt($string, $password = null, $chiper=null)
    {
        //Use ENV password if other is not set by an argument.
        if (is_null($password)) {
            $password=Env::get('app_key');
        }
        //Use default AES-256-ECB if no other chiper is specified
        if (is_null($chiper)) {
            $chiper="AES-256-ECB";
        }
        return base64_encode(openssl_encrypt($string, $chiper, $password));
    }


    /**
     * @param      $string
     * @param null   $password
     * @param null   $chiper
     *
     * @return string
     */
    public static function decrypt($string, $password = null, $chiper=null)
    {
        //Use ENV password if other is not set by an argument.
        if (is_null($password)) {
            $password=Env::get('app_key');
        }
        //Use default AES-256-ECB if no other chiper is specified
        if (is_null($chiper)) {
            $chiper="AES-256-ECB";
        }
        return openssl_decrypt(base64_decode($string), $chiper, $password);
    }

    /**
     * @param int $len
     * @param int $chars_type (1->all, 2->alpha numeric, 3->alphabet small,4->alphabet and capital ,5->numeric)
     *
     * @return string
     */
    public static function makePassword($len=16, $chars_type=1)
    {
        $password = array();
        //Default use all chars 0-9,a-z,A-Z,special chars
        if($chars_type==1) {
            $alphabet = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!#$%&()=?*+{}@';
        }
        else if($chars_type==2) {
            $alphabet = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890';
        }
        else if($chars_type==3) {
            $alphabet = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ';
        }
        else if($chars_type==4) {
            $alphabet = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
        }
        else if($chars_type==5) {
            $alphabet = '1234567890';
        }
        for ($i = 0; $i < $len; $i++) {
            $n = rand(0, strlen($alphabet) - 1);
            $password[] = $alphabet[$n];
        }
        return implode($password);
    }


    /**
     * @param      $password
     * @param null     $alg
     *
     * @return bool|string
     */
    public static function makeHash($password, $alg=null)
    {
        //At default we use password_hash which use bcrypt algorithm.
        if($alg==null) {
            $options = [
                'salt' => self::makePassword(30),
                'cost' => 10
            ];
            $hash=password_hash($password, PASSWORD_DEFAULT, $options);
        }else{
            //If alg is set then we use hash function
            $hash=hash($alg, $password);
        }
        return $hash;
    }

    /**
     * @param $password
     * @param $hash
     *
     * @return int
     */
    public static function verifyHash($password, $hash)
    {
        if (password_verify($password, $hash)) {
            return true;
        }
        else {
            return false;
        }
    }
}
