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

class SMail
{
    
    private $_to = '';
    private $_from = '';
    private $_from_name = '';
    private $_subject = '';
    private $_body = '';
    private $_disabled = false;

    public function __construct($to = '', $subject = '', $body = '') 
    {
        if($to) {
            $this->to($to);
        }

        if($subject) {
            $this->subject($subject);
        }

        if($body) {
            $this->body($body);
        }

        $mail = Env::get('mail');
        if(isset($mail->email)) {
            $this->_from = $mail->email;
        }
        if(isset($mail->name)) {
            $this->_from_name = $mail->name;
        }
        if(isset($mail->disabled)) {
            $this->_disabled = $mail->disabled;
        }
    }
    
    public function to($to) 
    {
        $this->_to = $to;
        return $this;
    }

    public function from($from) 
    {
        $this->_from = $from;
        return $this;
    }

    public function fromName($name) 
    {
        $this->_from_name = $name;
        return $this;
    }

    public function subject($s) 
    {
        $this->_subject = $s;
        return $this;
    }

    public function body($c) 
    {
        $this->_body = $c;
        return $this;
    }

    private function filterName($name) 
    {
        $rule = array("\r" => '',
                      "\n" => '',
                      "\t" => '',
                      '"'  => "'",
                      '<'  => '[',
                      '>'  => ']',
        );

        return trim(strtr($name, $rule));
    }
    
    public function send() 
    {
        if(!$this->_from) {
            throw new \Exception("Can't send email. From field is empty.");
        }
        if(!$this->_to) {
            throw new \Exception("Can't send email. To field is empty.");
        }
        if(!$this->_subject) {
            throw new \Exception("Can't send email. Subject is empty.");
        }
        if(!$this->_body) {
            throw new \Exception("Can't send email. Body is empty.");
        }

        $this->_from = filter_var($this->_from, FILTER_SANITIZE_EMAIL);
        $this->_to   = filter_var($this->_to, FILTER_SANITIZE_EMAIL);
        $this->_from_name = $this->filterName($this->_from_name);

        if(filter_var($this->_to, FILTER_VALIDATE_EMAIL) === false
            || filter_var($this->_from, FILTER_VALIDATE_EMAIL) === false
        ) {
            throw new \Exception("Invalid To ({$this->_to}) or From ({$this->_from}) email.");
        }

        $email = $this->_from;
        if($this->_from_name) {
            $email = "{$this->_from_name} <$email>";
        }
        
        $headers = 'From: $email' . "\r\n";
        $headers .= 'X-Mailer: PHP/SMailer' . "\r\n";

        if($this->_disabled) {
            return false;
        } else {
            return mail($this->_to, $this->_subject, $this->_body, $headers);
        }
    }
}