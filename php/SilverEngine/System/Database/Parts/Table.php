<?php

namespace Silver\Database\Parts;

use Silver\Database\Query;
use Silver\Database\Source;

class Table extends Part
{

    private $source;
    private $alias;

    // XXX: Parts\Raw not supported
    public function __construct($source, $alias = null) 
    {
        $this->source = $source;
        $this->alias = $alias;
    }

    protected static function compile($q) 
    {
        if ($q instanceof Source) {
            $ret = (string) $q->source->sourcePart();
            $ret .= ' as ' . $q->source->namePart();
        } else {
            // Old way for ::drop and ::create
            // This is using in relation join
            $ret = (string) Name::ensure($q->source);
            if ($q->alias) {
                $ret .= ' as ' . Name::ensure($q->alias);
            }
        }
        return $ret;
    }
}