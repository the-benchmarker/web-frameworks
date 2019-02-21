<?php

namespace Silver\Database\Traits;

use Silver\Database\Query;

trait QueryUnion
{
    private $unions = null;

    public function union($query) 
    {
        $this->addUnion($query, 'UNION');
        return $this;
    }

    public function unionAll($query) 
    {
        $this->addUnion($query, 'UNION ALL');
        return $this;
    }

    private function addUnion($query, $type) 
    {
        if(is_callable($query)) {
            $query = $query();
        }

        if(!($query instanceof Query)) {
            throw new \Exception("Wrong argument for union. '" . \gettype($query) . "' is not 'Query'.");
        }
        
        $this->unions[] = [$type, $query];
    }

    protected static function compileUnion($q) 
    {
        if($q->unions) {
            $r = '';
            foreach($q->unions as $union) {
                list($key, $query) = $union;
                $r .= " $key " . $query->toSql();
            }
            return $r;
        }
        return '';
    }
}
