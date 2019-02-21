<?php

namespace Silver\Database\Parts\Pgsql;

use Silver\Database\Parts\ColumnDef as P;
use Silver\Database\Parts\Column;

class ColumnDef extends P
{
    protected static function compile($q) 
    {
        $name = Column::ensure($q->name);

        if($q->getAutoincrement()) {
            $sql = $name
                 . self::compileType($q)
                 . self::compilePrimary($q);
        } else {
            $sql = $name
                 . self::compileType($q)
                 . self::compileUnsigned($q)
                 . self::compileNullable($q)
                 . self::compilePrimary($q)
                 . self::compileAutoInc($q)
                 . self::compileUnique($q)
                 . self::compileDefault($q);
        }

        if($index = self::compileReference($q, $name)) {
            $sql .= ', ' . $index;
        }

        return $sql;
    }

    protected static function compileType($q) 
    {
        if($q->getAutoincrement()) {
            return ' SERIAL';
        } else {
            return parent::compileType($q);
        }
    }

    protected static function mapType($type, $args) 
    {
        switch($type) {
        case 'mediumint':
            return ['integer', []];
        case 'datetime': // 2011-01-30 01:01:01
            return ['varchar', [19]];
        case 'year':
            return ['varchar', [4]];
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
