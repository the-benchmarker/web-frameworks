<?php

namespace Silver\Database\Traits;

use Silver\Database\Parts\Column;
use Silver\Database\Parts\ColumnList;

trait QueryGroupBy
{
    private $groupby = [];

    public function groupBy($column) 
    {
        $this->groupby[] = Column::ensure($column);
        return $this;
    }

    protected static function compileGroupBy($q) 
    {
        if (!empty($q->groupby)) {
            return " GROUP BY " . ColumnList::ensure($q->groupby);
        }
        return '';
    }
}
