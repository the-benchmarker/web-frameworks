<?php

namespace Silver\Database\Traits;

use Silver\Database\Parts\Table;
use Silver\Database\Parts\Column;
use Silver\Database\Source;

trait QueryFrom
{
    private $tables = [];

    public function from($table, $alias = null) 
    {

        if(is_callable($table)) {
            $table = $table();
        }

        // Make source from table
        $source = Source::make($table, $alias);
        $this->addSource($source);

        $table = Table::ensure($source);
        $this->tables[] = $table;
        return $this;
    }

    protected static function compileFrom($q) 
    {
        if($q->tables) {
            return ' FROM ' . implode(', ', $q->tables);
        }
        return '';
    }
}
