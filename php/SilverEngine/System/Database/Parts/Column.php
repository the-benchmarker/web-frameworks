<?php

namespace Silver\Database\Parts;

class Column extends Part
{

    protected $column;
    protected $table;
    protected $alias;
    protected $distinct = false;

    public function __construct($column_or_table, $column = null, $alias = null) 
    {
        // Set correct order
        if($column === null) {
            $column = $column_or_table;
            $table = null;
        } else {
            $table = $column_or_table;
        }

        // Split $column to $table.$column
        // we can skip $column spliting, by passing false for $table.
        // a.b.c => `a`.`b.c` (maybe should be beter split at last dot)
        if($table === null && is_string($column)) {
            $index = strrpos($column, '.');
            if($index !== false) {
                $table = substr($column, 0, $index);
                $column = substr($column, $index+1);
            }
        }

        if($alias === null && is_string($column)) {
            $index = strpos($column, ' ');
            if($index !== false) {
                $alias = substr($column, $index+1);
                $column = substr($column, 0, $index);
            }
        }

        $this->column = Name::ensure($column);
        $this->table = $table ? Name::ensure($table) : $table;
        $this->alias = $alias ? Name::ensure($alias) : $alias;
    }

    public function getName() 
    {
        return $this->column;
    }

    public function getTable() 
    {
        return $this->table;
    }

    public function getAlias() 
    {
        return $this->alias;
    }

    protected static function compile($q) 
    {
        $column = $q->column;
        $table = $q->table;
        $alias = $q->alias;

        if($q->distinct === true) {
            $ret = 'DISTINCT ';
        } else {
            $ret = '';
        }
        

        if($table) {
            $ret .= $table . ".";
        }

        $ret .= $column;

        if($alias) {
            $ret .= ' ' . $alias;
        }

        return $ret;
    }
}
