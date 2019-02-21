<?php

namespace Silver\Database\Parts\Sqlite;

use Silver\Database\Parts\ColumnDef as P;

class ColumnDef extends P
{
    protected static function compileAutoInc($q) 
    {
        return $q->autoincrement ? ' AUTOINCREMENT' : '';
    }

    protected static function mapType($type, $args) 
    {
        switch($type) {
        case 'enum':
            $max = max(array_map('strlen', $args));
            return ['varchar', [$max]];
        case 'set':
            // one,two,three
            $max = array_sum(array_map('strlen', $args));
            $max += count($args) - 1;
            return ['varchar', [$max]];
        default:
            return [$type, $args];
        }
    }
}