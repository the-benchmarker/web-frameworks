<?php

namespace Silver\Database\Query;

use Silver\Database\Query;
use Silver\Database\Parts\Table;
use Silver\Database\Parts\ColumnDef;

class Create extends Query
{

    private $table = null;
    private $schema = null;
    // populated by $schema callback
    private $columns = [];

    private $temporary = false;
    private $if_not_exists = false;
    private $autoinc_start = null;

    // Table options
    private $options = [];

    public function __construct($table_name, $schema) 
    {
        $this->table = $table_name;
        $this->schema = $schema;
    }

    public function column($name, $type, ...$args) 
    {
        $this->columns[] = $c = ColumnDef::ensure(array_merge([$name, $type], $args));
        return $c;
    }

    public function temporary($isit = true) 
    {
        $this->temporary = $isit;
        return $this;
    }

    public function ifNotExists($yes = true) 
    {
        $this->if_not_exists = $yes;
        return $this;
    }

    public function option($key, $value) 
    {
        $key = strtoupper($key);
        if($value === null) {
            unset($this->options[$key]);
        } else {
            $this->options[$key] = $value;
        }
        return $this;
    }
    
    public function engine($engine) 
    {
        return $this->option('engine', $engine);
    }

    public function charset($charset) 
    {
        $this->option('default character set', null);
        return $this->option('character set', $charset);
    }

    public function defaultCharset($charset) 
    {
        $this->option('character set', null);
        return $this->option('default character set', $charset);
    }

    // XXX: mysql only
    public function autoincrement($first) 
    {
        return $this->option('auto_increment', $first);
    }

    public function comment($comment) 
    {
        return $this->option('comment', \Query::quote($comment));
    }

    /* Columns */
    public function boolean($name) 
    {
        return $this->column($name, 'boolean');
    }

    public function enum($name, ...$enum) 
    {
        return $this->column($name, 'enum', ...$enum);
    }

    public function set($name, ...$set) 
    {
        return $this->column($name, 'set', ...$set);
    }

    // Numeric
    public function smallInt($name) 
    {
        return $this->column($name, 'smallint');
    }

    public function mediumInt($name) 
    {
        return $this->column($name, 'mediumint');
    }

    public function integer($name) 
    {
        return $this->column($name, 'integer');
    }

    public function bigInt($name) 
    {
        return $this->column($name, 'bigint');
    }

    public function decimal($name, $precision, $scale) 
    {
        return $this->column($name, 'decimal', $precision, $scale);
    }

    // Text
    public function varchar($name, $size) 
    {
        return $this->column($name, 'varchar', $size);
    }

    public function text($name) 
    {
        return $this->column($name, 'text');
    }

    // Time
    public function timestamp($name) 
    {
        return $this->column($name, 'timestamp');
    }

    public function time($name) 
    {
        return $this->column($name, 'time');
    }

    public function date($name) 
    {
        return $this->column($name, 'date');
    }

    public function datetime($name) 
    {
        return $this->column($name, 'datetime');
    }

    public function year($name) 
    {
        return $this->column($name, 'year');
    }
    /* --- */

    protected static function compile($q) 
    {
        $table = Table::ensure($q->table);

        $sql = 'CREATE';
        if($q->temporary) {
            $sql .= ' TEMPRARY';
        }
        $sql .= ' TABLE';
        if($q->if_not_exists) {
            $sql .= ' IF NOT EXISTS';
        }
        $sql .= ' ' . $table;

        if($q->schema instanceof \Parts\Table) {
            $sql .= ' LIKE ' . $q->schema;
        } elseif($q->schema instanceof \Query\Select) {
            $sql .= ' AS ' . $q->schema->toSql();
        } elseif(is_callable($q->schema)) {

            $schema = $q->schema;
            $q->columns = [];
            $schema($q);

            // Columns
            $parts = [];
            foreach ($q->columns as $c) {
                $parts[] = $c->toSql();
            }
            $sql .= ' (' . implode(', ', $parts) . ')';

            if(count($q->options) > 0) {
                foreach($q->options as $k=>$v) {
                    $sql .= ' ' . $k . '=' . $v;
                }
            }
        } else {
            throw new \Exception("Unknown schema type.");
        }

        return $sql;
    }
}
