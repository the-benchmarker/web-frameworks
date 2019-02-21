<?php

namespace Silver\Database\Parts;

use Silver\Database\QueryObject;

class Name extends Part
{

    private $name;

    public function __construct($name) 
    {
        // XXX: check if it is source
        // If source get table name of it
        if (is_object($name) && is_subclass_of(get_class($name), \Silver\Database\Source::class)) {
            $name = $name->table();
        }
        $this->name = trim($name);
    }

    public function string() 
    {
        return $this->name;
    }

    protected static function quoteChar() 
    {
        return '`';
    }

    protected static function compile($q) 
    {
        return (string) new Quote($q->name, static::quoteChar());
    }
}