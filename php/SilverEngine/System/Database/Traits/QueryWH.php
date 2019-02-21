<?php

namespace Silver\Database\Traits;

use Silver\Database\Parts\Filter;
use Silver\Database\Parts\Parts;
use Silver\Database\Parts\Raw;
use Silver\Database\Parts\Value;
use Silver\Database\Parts\Literal;

trait QueryWH
{

    private $where = [];
    private $having = [];

    public function where($column, $operator=null, $value=null, $how='and', $not=false) 
    {
        return $this->cond('where', $column, $operator, $value, $how, $not);
    }

    public function having($column, $operator=null, $value=null, $how='and', $not=false) 
    {
        return $this->cond('having', $column, $operator, $value, $how, $not);
    }

    private function cond($cond, $column, $operator, $value, $how, $not) 
    {
        $how = strtoupper($how);
        if(!($how == 'AND' || $how == 'OR')) {
            throw new \Exception("Unknown boolean operator '$how'");
        }

        if(is_callable($column)) {
            $pfn = $cond . 'Paren';
            return $this->$pfn($column, $how, $not);
        }

        list($operator, $value) = $this->prepareOperatorValue($operator, $value);

        if(is_callable($value)) {
            $value = $value();
        }

        if($operator == 'BETWEEN') {
            // Value must be [1, 2] array
            list($from, $to) = $value;
            $filter = new Filter(
                $column,
                $operator,
                new Parts(Value::ensure($from), 'AND', Value::ensure($to)),
                $not
            );
        } else {
            $filter = new Filter($column, $operator, $value, $not);
        }

        /**
 * XXX 
         * Postgresql support doesn't support referencing to
         * agregate columns in having.
         */
        // For PostgreSQL
        // $filter = static::processFilter($cond, $filter)
        /*
         * if(having) {
         *   $filter->col = Query::current?()->getColumn($filter->col)->definition
         * }
         */

        if($this->$cond) {
            $this->$cond = new Parts($this->$cond, $how, $filter);
        } else {
            $this->$cond = $filter;
        }

        return $this;
    }

    private function prepareOperatorValue($operator, $value) 
    {
        if($value === null) {
            if($operator instanceof \Query || is_array($operator)) {
                return ['IN', $operator];
            }
            if(is_bool($operator)) {
                return ['=', Literal::ensure($operator)];
            }
            if($operator === null) {
                return ['IS', Literal::null()];
            }
            return ['=', $operator];
        } else {
            return [strtoupper($operator), $value];
        }
    }

    private function havingParen($cb, $how = 'and', $not = false) 
    {
        return $this->parent('having', $cb, $how, $not);
    }

    private function whereParen($cb, $how = 'and', $not = false) 
    {
        return $this->parent('where', $cb, $how, $not);
    }

    private function paren($cond, $cb, $how, $not) 
    {
        $cond = $this->$cond;
        $this->$cond = null;

        $cb($this);

        if($cond === null) {
            if($not) {
                $this->$cond = new Parts('NOT', new Paren($this->$cond));
            } else {
                $this->$cond = new Paren($this->$cond);
            }
        } else {
            if($not) {
                $this->$cond = new Parts($cond, $how, 'NOT', new Paren($this->$cond));
            } else {
                $this->$cond = new Parts($cond, $how, new Paren($this->$cond));
            }
        }
        return $this;
    }

    public function __call($str, $args) 
    {
        $orig = $str;

        $cond = null;
        $column = null;
        $operator = null;
        $value = null;
        $how = 'and';
        $not = false;

        if(stripos($str, 'or') === 0) {
            $str = substr($str, 2);
            $how = 'or';
        }

        if(stripos($str, 'not') === 0) {
            $str = substr($str, 3);
            $not = true;
        }

        if(stripos($str, 'where') === 0) {
            $str = substr($str, 5);
            $cond = 'where';
        } elseif(stripos($str, 'having') === 0) {
            $str = substr($str, 6);
            $cond = 'having';
        }

        if(strlen($str) > 0) {
            $column = self::snake_case($str);
        }

        if($cond) {
            if(!$column) { $column = array_shift($args);
            }
            $operator = array_shift($args);
            $value = array_shift($args);

            return $this->$cond($column, $operator, $value, $how, $not);
        } else {
            throw new \Exception('Undefined method ' . static::class . '::' . $orig);
        }
    }

    public static function compileWhere($q) 
    {
        if($q->where) {
            return ' WHERE ' . $q->where;
        }
        return '';
    }

    public static function compileHaving($q) 
    {
        if($q->having) {
            return ' HAVING ' . $q->having;
        }
        return '';
    }

    private static function snake_case($str) 
    {
        $out = '';
        $str = lcfirst($str);
        for($i=0; $i < strlen($str); $i++) {
            $chr = $str[$i];
            if ('A' <= $chr && $chr <= 'Z') {
                $out .= '_' . strtolower($chr);
            } else {
                $out .= $chr;
            }
        }
        return $out;
    }
}