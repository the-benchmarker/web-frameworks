<?php

namespace Silver\Database\Query\Mysql;

use Silver\Database\Query\Drop as P;

class Drop extends P
{

    protected static function compile($q) 
    {
        if (self::getTxCounter()) {
            throw new \Exception("DDL statements are not allowed during the transaction.");
        }
        return parent::compile($q);
    }
}