<?php

namespace Silver\Database\Traits;

trait QueryLimit
{
    private $limit = null;
    private $offset = null;

    public function limit($count) 
    {
        $this->limit = (int) $count;
        return $this;
    }

    public function getLimit() 
    {
        return $this->limit;
    }

    public function offset($offset) 
    {
        if($this->limit === null) {
            $this->limit = 1;
        }
        $this->offset = (int) $offset;
        return $this;
    }

    public function page($page, $per_page = null) 
    {
        if($per_page !== null) {
            $this->limit = $per_page;
        }
        
        if($this->limit === null) {
            throw new \Exception("Limit (page size) must be set!");
        }

        if($page < 1) {
            throw new \Exception("Don't know how to show $page. page");
        }
        
        $this->offset = $this->limit * ($page - 1);

        return $this;
    }

    protected static function compileLimit($q) 
    {
        if($q->limit) {
            $r = ' LIMIT ' . $q->limit;
            if($q->offset) {
                $r .= ' OFFSET ' . $q->offset;
            }
            return $r;
        }
        return '';
    }
}
