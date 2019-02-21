<?php

namespace Silver\Database\Parts;

class BackQuote extends Quote
{
    public function __construct($value) 
    {
        parent::__construct($value, '`');
    }
}
