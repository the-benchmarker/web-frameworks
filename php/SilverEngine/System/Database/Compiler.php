<?php

namespace Silver\Database;

use Silver\Database\Query;

trait Compiler
{
    // Current queries stack
    private static $qstack = [];

    // Should be abstract, but php says: (E_STRICT)
    // Static function Silver\Database\Compiler::compile() should not be abstract
    protected static function compile($q) 
    {
        throw new \Exception("This should be abstract method. Do not call it directly.");
    }

    public static function current() 
    {
        if(self::$qstack) {
            return self::$qstack[0];
        } else {
            return null;
        }
    }

    private static function parentQuery() 
    {
        if(count(self::$qstack) >= 2) {
            return self::$qstack[1];
        }
        return null;
    }

    public function toSql() 
    {
        try {
            array_unshift(self::$qstack, $this);
            $class = get_called_class();
            $dialect = ucfirst(Db::driverName());
            $pos = strrpos($class, '\\') ?: 0;

            $new = substr_replace($class, '\\' . $dialect, $pos, 0);

            // Remove current bindings
            // (Query can be reused)
            if($this instanceof Query) {
                $this->clearBindings();
            }

            $sql = class_exists($new)
                 ? $new::compile($this)
                 : $class::compile($this);

            // Add bindings to parent query
            if($this instanceof Query) {
                if($p = self::parentQuery()) {
                    if($b = $this->getBindings()) {
                        $p->bind($b);
                    }
                }
            }

            return $sql;
        } finally {
            array_shift(self::$qstack);
        }
    }

    public function __toString() 
    {
        try {
            return $this->toSql();
        } catch (\Exception $e) {
            echo "ERROR __toString(): " . $e->getMessage() . "\n";
            exit;
        }
    }
}

