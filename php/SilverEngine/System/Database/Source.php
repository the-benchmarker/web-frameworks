<?php

namespace Silver\Database;

use Silver\Database\Source\Table as TableSource;
use Silver\Database\Source\Model as ModelSource;
use Silver\Database\Source\Query as QuerySource;
use Silver\Database\Query;
use Silver\Database\Parts\Name;

abstract class Source
{
    private $name;

    protected function __construct($name) 
    {
        if (empty($name)) {
            throw new \Exception("Source must have a name.");
        }
        $this->name = $name;
    }

    public function name() 
    {
        return $this->name;
    }

    abstract public function primary();
    abstract public function table();
    abstract public function sourcePart();

    public function namePart() 
    {
        return Name::ensure($this->name());
    }

    public function reference($refName, Query $q, $alias, $args=[]) 
    {
        throw new \Exception('Source ' . static::class . ' does not supports references.');
    }

    public function relation($relName, $alias) 
    {
        throw new \Exception('Source ' . static::class . ' does not supports relations.');
    }

    public static function make($source, $alias = null) 
    {
        if (class_exists($source) and is_subclass_of($source, QueryObject::class)) {
            return new ModelSource($source, $alias);
        } else if (is_string($source)) {
            return new TableSource($source, $alias);
        } else if ($source instanceof Query) {
            return new QuerySource($source, $alias);
        } else {
            throw new Exception("Wrong class name '$model'"); // Should be QueryException
        }
    }
}
