<?php

namespace Silver\Database\Test;

use Silver\Database\Query;

class Test
{

    private static $tests = [];
    private static $data = [];
    private static $gonly;

    private $name;
    private $query;
    private $result;
    private $only;

    public static function init() 
    {
        self::inc('data.php');
        self::inc('create.php');
        self::inc('insert.php');
        self::inc('select.php');
        self::inc('alter.php');

        Query::connect('sqlite', 'sqlite:/tmp/baza.sqlite');
        Query::connect('mysql',  'mysql:host=localhost;dbname=matija;charset=utf8', 'matija', 'geslo');
        Query::connect('pgsql',  'pgsql:host=localhost;dbname=matija', 'matija', null);
    }

    public static function runAll() 
    {
        foreach (Query::connections() as $c) {
            echo "\n";
            echo "                              ~~  " . strtoupper($c) . " ~~\n";
            try {
                self::runOne($c);
            } catch (\Exception $e) {
                echo "Fatal: " . $e->getMessage() . "\n";
            }
        }
    }

    public static function runOne($dbname) 
    {
        $counter = [
            true => 0,
            false => 0,
            3 => 0,
        ];
        Query::withConnection(
            $dbname, function () use (&$counter) {
                foreach (self::$tests as $test) {
                    $success = $test->run();
                    $counter[$success]++;
                }
            }
        );
        echo "\n";
        echo "     Success "
            . $counter[true]
            . " failed "
            . $counter[false]
            . " skipped "
            . $counter[3]
            . "\n";
    }

    public static function define($query, $result = null, $name = null, $only = null) 
    {
        self::$tests[] = new Test($name, $query, $result, $only);
    }

    public static function on($dbs, $cb) 
    {
        $prev = self::$gonly;
        self::$gonly = $dbs;
        $cb();
        self::$gonly = $prev;
    }

    public static function data($key, $value = null) 
    {
        if ($value !== null) {
            self::$data[$key] = $value;
        }
        return self::$data[$key];
    }

    public function __construct($name, $query, $result) 
    {
        $this->query = $query;
        $this->name = $name;
        $this->result = $result;
        $this->only = self::$gonly;
    }

    private function run() 
    {
        if ($this->only) {
            if(!in_array(Query::driverName(), $this->only)) {
                return 3;
            }
        }

        echo " * ";
        if ($this->name) {
            echo $this->name . ': ';
        }

        try {
            if ($this->result) {
                $r = $this->query->fetchAll()[0];

                if (!$this->compare((array) $r, $this->result)) {
                    echo "wrong-result\n";
                    $self = $this;
                    self::withIndent(
                        function () use ($self, $r) {
                            echo "-----\n";
                            echo $self->query->toSql() . "\n";
                            if ($bds = $self->query->getBindings()) {
                                echo print_r($bds, true) . "\n";
                            }
                            echo "-----\n";
                            echo "GET:  " . print_r((array) $r, true);
                            echo "WANT: " . print_r((array) $self->result, true);
                            echo "+----\n";
                        }, 5, '| '
                    );
                    return false;
                }
            } else {
                $this->query->execute();
            }
            echo 'ok';
        } catch (\Exception $e) {
            echo "fail\n";
            echo "----+\n";
            echo "ERROR: " . $e->getMessage() . "\n";
            $q = $this->query->toSql();
            echo "SQL: " . (is_array($q) ? print_r($q, 1) : ($q."\n"));
            if ($bnds = $this->query->getBindings()) {
                echo var_dump($bnds, true);
            }
            echo "----+\n";
            return false;
        }
        echo "\n";
        return true;
    }

    private function compare($data, $must) 
    {
        foreach($must as $key => $val) {
            if(!array_key_exists($key, $data)) {
                return false;
            }
            if($data[$key] != $val) {
                return false;
            }
        }
        return true;
    }

    private function withIndent($cb, $cols = 5, $prepend = '') 
    {
        ob_start();
        $cb();
        $res = ob_get_contents();
        ob_get_clean();
        $res = explode("\n", $res);
        $res = array_map(
            function ($line) use ($cols, $prepend) {
                return str_repeat(' ', $cols) . $prepend . $line;
            }, $res
        );
        echo implode("\n", $res);
    }

    private static function inc($file) 
    {
        return include dirname(__FILE__) . '/' . $file;
    }
}