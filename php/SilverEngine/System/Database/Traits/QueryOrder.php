<?php

namespace Silver\Database\Traits;

use Silver\Database\Parts\Name;

trait QueryOrder
{
    private $order = [];

    public function orderBy($column, $dir = 'asc') 
    {
        $column = Name::ensure($column);

        $dir = strtoupper($dir);
        if(!($dir == 'ASC' || $dir == 'DESC')) {
            throw new \Exception("Unknown order direction '$dir'");
        }

        $this->order[] = [$column, $dir];
        return $this;
    }

    protected static function compileOrder($q) 
    {
        if(count($q->order)) {
            $parts = array_map(
                function ($p) {
                    return $p[0] . ' ' . $p[1];
                }, $q->order
            );
            return ' ORDER BY ' . implode(', ', $parts);
        }
        return '';
    }

}
