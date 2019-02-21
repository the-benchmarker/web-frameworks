<?php

namespace Silver\Database\Parts;

class Fn extends Part
{

    private $name;
    private $args;

    public static function count($column = null) 
    {
        if ($column === null) {
            $column = Literal::wild();
        }
        return static::ensure(['COUNT', Column::ensure($column)]);
    }

    public static function groupConcat($column, $sep = ',') 
    {
        return static::ensure(
            [
            'GROUP_CONCAT',
            Column::ensure($column),
            Literal::ensure($sep)
            ]
        );
    }

    public function __construct($name, ...$args) 
    {
        $this->name = Raw::ensure($name);
        $this->args = [];
        foreach ($args as $arg) {
            $this->args[] = Literal::ensure($arg);
        }
    }

    public static function __callStatic($name, $args) 
    {
        $args = array_merge([$name], $args);
        return static::ensure($args);
    }

    protected static function mapFn($fn, $args) 
    {
        return [$fn, $args];
    }

    public static function compile($q) 
    {
        list ($name, $args) = static::mapFn($q->name, $q->args);
        return $name . '(' . implode(', ', $args) . ')';
    }
}