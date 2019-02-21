<?php

namespace Silver\Database\Parts\Pgsql;

use Silver\Database\Parts\Fn as P;
use Silver\Database\Parts\Column;
use Silver\Database\Parts\Literal;
use Silver\Database\Parts\Parts;
use Silver\Database\Parts\Raw;

class Fn extends P
{

    protected static function mapFn($fn, $args) 
    {
        switch($fn) {
        case 'GROUP_CONCAT':
            return [
                'string_agg',
                [
                    // Cast into string
                    Parts::ensure(
                        [
                        Raw::ensure("'' ||"),
                        $args[0],

                        ]
                    ),
                    $args[1]
                ]
            ];
        default:
            return [$fn, $args];
        }
    }

}