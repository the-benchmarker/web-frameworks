<?php

namespace Silver\Database\Query\Sqlite;

use Silver\Database\Query\Alter as P;

class Alter extends P
{
    protected static function compileModify($c, $add) 
    {
        throw new \Exception("Sqlite doesn't support modifying columns.");
    }

    protected static function compileChange($c, $add) 
    {
        throw new \Exception("Sqlite doesn't support changing columns.");
    }
}