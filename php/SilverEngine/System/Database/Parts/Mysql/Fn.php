<?php

namespace Silver\Database\Parts\Mysql;

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
                'GROUP_CONCAT',
                [
                    Parts::ensure(
                        [
                        $args[0],
                        Raw::ensure('SEPARATOR'),
                        $args[1]
                        ]
                    )
                ]
            ];
        default:
            return [$fn, $args];
        }
    }
}