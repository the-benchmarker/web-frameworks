<?php

namespace Silver\Database\Parts\Pgsql;

use Silver\Database\Parts\Name as P;

class Name extends P
{

    protected static function quoteChar() 
    {
        return '"';
    }

}