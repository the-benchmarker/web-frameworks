<?php

namespace Silver\Database\Parts;

class Parts extends Part
{

    private $parts;

    public function __construct(...$args) 
    {
        $this->parts = array_map(
            function ($arg) {
                return Raw::ensure($arg);
            }, $args
        );
    }

    protected static function compile($q) 
    {
        return implode(' ', $q->parts);
    }
}