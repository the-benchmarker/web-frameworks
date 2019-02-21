<?php

namespace Silver\Database\Parts;

class SingleQuote extends Quote
{
    public function __construct($value) 
    {
        parent::__construct($value, "'");
    }
}
