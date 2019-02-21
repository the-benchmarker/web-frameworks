<?php

namespace Silver\Database\Parts;

class Filter extends Parts
{
    private $condition;
    private $not = false;
    
    public function __construct($column, $op, $value, $not = false) 
    {
        $this->condition = Parts::ensure(
            [
            Column::ensure($column),
            Raw::ensure($op),
            Value::ensure($value),
            ]
        );

        $this->not = $not;
    }

    protected static function compile($q) 
    {
        $ret = (string) $q->condition;

        if($q->not) {
            $ret = 'NOT ' . $ret;
        }

        return $ret;
    }
}
