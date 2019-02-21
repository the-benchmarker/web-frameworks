<?php

namespace Silver\Database;

use Silver\Database\Query\Drop;
use Silver\Database\Parts\Fn;
use Silver\Database\Parts\Column;

abstract class Query extends Db
{
    private $bindings = [];
    private $sources = [];

    use Compiler;

    /**
     * @param array ...$columns
     * @return mixed
     */
    public static function select(...$columns) 
    {
        return self::instance('select', [$columns]);
    }

    /**
     * @param string $column
     * @return mixed
     */
    public static function count($column = 'count') 
    {
        return self::select(
            Column::ensure(
                [
                null,
                Fn::count(),
                $column
                ]
            )
        );
    }

    /**
     * @param array ...$columns
     * @return mixed
     */
    public static function delete(...$columns) 
    {
        return self::instance('delete', [$columns]);
    }

    /**
     * @param $table
     * @param array $updates
     * @return mixed
     */
    public static function update($table, $updates = []) 
    {
        return self::instance('update', [$table, $updates]);
    }

    /**
     * @param $table
     * @param null  $data
     * @return mixed
     */
    public static function insert($table, $data = null) 
    {
        return self::instance('insert', [$table, $data]);
    }

    /**
     * @param $table
     * @param $cb
     * @return mixed
     */
    public static function create($table, $cb) 
    {
        return self::instance('create', [$table, $cb]);
    }

    /**
     * @param $table
     * @return mixed
     */
    public static function drop($table) 
    {
        return self::instance('drop', [$table]);
    }

    /**
     * @param $table
     * @param null  $cb
     * @return mixed
     */
    public static function alter($table, $cb = null) 
    {
        return self::instance('alter', [$table, $cb]);
    }

    /**
     * @param $type
     * @param array $args
     * @return mixed
     */
    protected static function instance($type, $args = []) 
    {
        $class = 'Silver\\Database\\Query\\' . ucfirst($type);
        return new $class(...$args);
    }

    /**
     * @param $value
     */
    public function bind($value) 
    {
        if(is_array($value)) {
            $this->bindings = array_merge($this->bindings, $value);
        } else {
            $this->bindings[] = $value;
        }
    }

    /**
     * @return array
     */
    public function getBindings() 
    {
        return $this->bindings;
    }

    /**
     *
     */
    public function clearBindings() 
    {
        $this->bindings = [];
    }

    public function addSource($source) 
    {
        $this->sources[$source->name()] = $source;
    }

    public function getSource($name) 
    {
        if (isset($this->sources[$name])) {
            return $this->sources[$name];
        }
        return null;
    }

    public function getSourceByModel($class) 
    {
        foreach($this->sources as $source) {
            if ($source instanceof \Silver\Database\Source\Model) {
                if ($source->model() == $class) {
                    return $source;
                }
            }
        }
        return null;
    }
}
