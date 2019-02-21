<?php

namespace Silver\Database\Query;

use Silver\Database\Query;
use Silver\Database\Traits\QueryWH;
use Silver\Database\Parts\Name;
use Silver\Database\Parts\Column;
use Silver\Database\Parts\Value;
use Silver\Database\Source;
use Silver\Database\Parts\Table;

class Update extends Query
{
    use QueryWh;

    private $table;
    private $updates = [];

    public function __construct($table, $updates = []) 
    {
        $source = Source::make($table);
        $this->addSource($source);

        $this->table = Table::ensure($source);

        foreach(array_keys($updates) as $key) {
            $value = $updates[$key];
            if(is_array($value)) {
                $key = $value[0];
                $value = $value[1];
            }
            $this->set($key, $value);
        }
    }

    public function set($column, $value) 
    {
        $this->updates[] = [Column::ensure($column), Value::ensure($value)];
        return $this;
    }

    protected static function compile($q) 
    {
        $sql = 'UPDATE ' . $q->table . ' SET ';

        $sql .= implode(
            ', ', array_map(
                function ($cv) {
                    list($col, $val) = $cv;
                    return "$col = $val";
                }, $q->updates
            )
        );

        return $sql . static::compileWhere($q);
    }

    public function having(...$args) 
    {
        throw new \Exception('Cannot use having in update statement.');
    }
}
