<?php

namespace Silver\Database\Parts;

class DoubleQuote extends Quote
{
    public function __construct($value) 
    {
        parent::__construct($value, '"');
    }
}
