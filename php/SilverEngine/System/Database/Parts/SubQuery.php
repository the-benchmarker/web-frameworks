<?php

namespace Silver\Database\Parts;

use Silver\Database\Query;

class SubQuery extends Part
{

    private $query;

    public function __construct(Query $q) 
    {
        $this->query = $q;
    }

    protected static function compile($q) 
    {
        $ret = '(' . $q->query->toSql() . ')';
        if($c = Query::current()) {
            $c->bind($q->query->getBindings());
        }
        return $ret;
    }
}