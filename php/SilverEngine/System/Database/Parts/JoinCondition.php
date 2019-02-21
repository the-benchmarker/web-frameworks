<?php

namespace Silver\Database\Parts;

class JoinCondition extends Part
{

    private $column;
    private $condition;

    public function __construct($col1, $operator = null, $col2 = null) 
    {
        // Default = operator
        if($col2 === null && $operator !== null) {
            $col2 = $operator;
            $operator = '=';
        }
        
        if($col2 === null) {
            $this->column = Name::ensure($col1);
        } else {
            // FIXME: use Filter
            $this->condition = Parts::ensure(
                [
                Column::ensure($col1),
                Raw::ensure($operator),
                Column::ensure($col2)
                ]
            );
        }
    }

    protected static function compile($q) 
    {
        if($q->condition !== null) {
            return 'ON ' . $q->condition;
        } else {
            return 'USING (' . $q->column . ')';
        }
    }
}
