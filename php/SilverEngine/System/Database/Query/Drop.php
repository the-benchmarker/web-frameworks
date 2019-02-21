<?php

namespace Silver\Database\Query;

use Silver\Database\Query;
use Silver\Database\Parts\Table;

class Drop extends Query
{
    
    private $table = null;
    private $if_exists = false;

    public function __construct($table) 
    {
        $this->table = Table::ensure($table);
    }

    public function ifExists($yes = true) 
    {
        $this->if_exists = $yes;
        return $this;
    }

    protected static function compile($q) 
    {
        $sql = 'DROP TABLE';
        if($q->if_exists === true) {
            $sql .= ' IF EXISTS';
        }
        $sql .= ' ' . $q->table;
        return $sql;
    }
}