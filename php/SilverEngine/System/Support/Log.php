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


class Log
{

    private static $types = [
        'info',
        'ok',
        'warning',
        'error',
        'api',
        'db',
        'start',
        'end',
        'debug',
        'normal',
        'danger',
        'aboard',
        'finish',
        'url',
    ];


    /**
     * @param $method
     * @param $args
     * @throws \Exception
     */
    public function __call($method, $args) 
    {
        if(array_search($method, self::$types) === false) {
            throw new \Exception("Undefined method Log::$method try allowed types [ ". implode(', ', self::$types)." ]");
        }
            $this->create($args[0], $method);
    }

    /**
     * @param $message
     * @param $type
     * @throws \Exception
     */
    private function create($message, $type)
    {
        //        dd($type);
        $ip   = $_SERVER["REMOTE_ADDR"];

        $path = ROOT . "Storage/Logs/" . date("Y-m-d") . ".log";
        $fp = fopen($path, "a+");

        $line = "[".date("Y-m-d H:i:s")."][ ".$type ." ]\t$ip\t$message\r\n";
        //        $line = "{$message}\r\n";

        //        dd($line);

        if($fp) {
            fwrite($fp, $line);
            fclose($fp);
            return false;
        } else {
            throw new \Exception("Unable to write to file $path.");
        }
    }
}
