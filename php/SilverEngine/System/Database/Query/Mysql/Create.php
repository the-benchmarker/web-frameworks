<?php

namespace Silver\Database\Query\Mysql;

use Silver\Database\Query\Create as P;

class Create extends P
{

    protected static function compile($q) 
    {
        if (self::getTxCounter()) {
            throw new \Exception("DDL statements are not allowed during the transaction.");
        }
        return parent::compile($q);
    }
}