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

namespace Silver\Exception;

class Exception extends \Exception
{
    public function __construct($message = null, $code = 0, Exception $previous = null) 
    {
        parent::__construct($message, $code, $previous);
    }


    public function setFile($file) 
    {
        $this->file = $file;
    }

    public function setLine($line) 
    {
        $this->line = $line;
    }

    public function setTrace($trace) 
    {
        $this->trace = $trace;
    }
}