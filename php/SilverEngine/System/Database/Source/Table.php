<?php

namespace Silver\Database\Source;

use Silver\Database\Source;
use Silver\Database\Parts\Name;

class Table extends Source
{
    private $table;
    
    protected function __construct($table, $name = null) 
    {
        // Get alias from name
        if ($name === null && is_string($table)) {
            $index = strpos($table, ' ');
            if($index !== false) {
                $name = substr($table, $index+1);
                $table = substr($table, 0, $index);
            }
        }

        parent::__construct($name ?: $table);
        $this->table = $table;
    }

    public function primary() 
    {
        return 'id';
    }

    public function table() 
    {
        return $this->table;
    }

    public function sourcePart() 
    {
        return Name::ensure($this->table());
    }
}