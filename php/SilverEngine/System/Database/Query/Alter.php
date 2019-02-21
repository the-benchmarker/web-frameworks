<?php

namespace Silver\Database\Query;

use Silver\Database\Query;
use Silver\Database\Parts\Table;
use Silver\Database\Parts\ColumnDef;
use Silver\Database\Parts\Name;

class Alter extends Query
{

    private $table;
    private $cb;

    private $qs = [];

    public function __construct($table, $cb = null) 
    {
        $this->table = $table;
        $this->cb = $cb;
    }

    private function addQ($type, $data) 
    {
        $this->qs[] = [$type, $data];
        return $data;
    }

    public function addColumn($name, $type, ...$args) 
    {
        return $this->addQ('add', ColumnDef::ensure(array_merge([$name, $type], $args)));
    }

    public function modifyColumn($name, $type, ...$args) 
    {
        return $this->addQ('modify', ColumnDef::ensure(array_merge([$name, $type], $args)));
    }

    public function changeColumn($old_name, $new_name, $type, ...$args) 
    {
        $this->addQ(
            'change', [
            Name::ensure($old_name),
            $c = ColumnDef::ensure(array_merge([$new_name, $type], $args))
            ]
        );
        return $c;
    }

    public function dropColumn($name) 
    {
        return $this->addQ('drop', Name::ensure($name));
    }

    // Execute multiple statements
    public function execute() 
    {
        $qs = $this->toSql();

        foreach($qs as $sql) {
            if($this->isDebug()) {
                echo "DEBUG: $sql\n";
            }
            static::exec($sql);
        }
    }

    protected static function compile($q) 
    {
        $table = Name::ensure($q->table);
        $q->qs = [];

        if($cb = $q->cb) {
            $cb($q);
        }

        $qs = [];
        foreach($q->qs as $q) {
            list($type, $data) = $q;
            $fn = 'compile' . ucfirst($type);
            static::$fn(
                $data, function ($sql) use (&$qs, $table) {
                    $qs[] = "ALTER TABLE $table " . $sql;
                }
            );
        }

        return $qs;
    }

    protected static function compileAdd($c, $add) 
    {
        $add("ADD $c");
    }

    protected static function compileModify($c, $add) 
    {
        $add("MODIFY $c");
    }

    protected static function compileChange($c, $add) 
    {
        list($old, $newdef) = $c;
        $add("CHANGE $old $newdef");
    }

    protected static function compileDrop($c, $add) 
    {
        $add("DROP $c");
    }

    // Forbidden
    public function bind($v) 
    {
        throw new \Exception("Cannot bind Value to alter query.");
    }
}
