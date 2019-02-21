<?php

namespace Silver\Database\Traits;

use Silver\Database\Parts\Table;
use Silver\Database\Parts\JoinCondition;
use Silver\Database\Source;
use Silver\Database\Parts\Column;

trait QueryJoin
{
    private $joins = [];

    public function join($table, $condition = []) 
    {
        return $this->doJoin($table, $condition, 'inner');
    }

    public function leftJoin($table, $condition = []) 
    {
        return $this->doJoin($table, $condition, 'left');
    }

    public function rightJoin($table, $condition = []) 
    {
        return $this->doJoin($table, $condition, 'right');
    }

    public function ref($ref, ...$args) 
    {
        $column = Column::ensure($ref);

        $source = $this->getSource($column->getTable()->string());
        if ($source === null) {
            throw new \Exception("Source '$class' not defined.");
        }

        $query = $source->reference(
            $column->getName()->string(),
            $this,
            $column->getAlias() ? $column->getAlias()->string() : null,
            $args
        );

        if ($query instanceof \Silver\Database\Query) {
            return $query;
        } else {
            return $this;
        }
    }

    private function doJoin($table, $condition, $type) 
    {
        if (is_string($table) && strpos($table, '.') != false) {
            // XXX: ignored condition
            $column = Column::ensure($table);
            // getName, getTable, getAlias
            $source = $this->getSource($column->getTable()->string());
            if ($source === null) {
                throw new \Exception("Source '$class' not defined.");
            }

            // TODO: name if not alias
            $relation = $source->relation($column->getName()->string(), $column->getAlias()->string());

            $this->joins[] = [
                $relation->getTable(),
                $relation->getJoinCondition(),
                $type
            ];
        } else {
            $source = Source::make($table);
            $this->addSource($source);
            $table = Table::ensure($source);
            $condition = JoinCondition::ensure($condition);
        
            $this->joins[] = [$table, $condition, $type];
        }
        return $this;
    }

    protected static function compileJoin($q) 
    {
        if($q->joins) {
            $ret = array_map(
                function ($join) {
                    list ($table, $cond, $type) = $join;
                    $type = strtoupper($type);
                    return " $type JOIN $table $cond";
                }, $q->joins
            );
            return implode(' ', $ret);
        }
        return '';
    }
}
