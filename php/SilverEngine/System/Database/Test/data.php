<?php

namespace Silver\Test\Database;

Test::data('weird', 'weird\'"`name');
Test::data(
    'data', [
    1 => [
        'id_main' => 1, // XXX autoinc
        'boolean' => 1, // FIXME: dialect
        'smallInt' => 1,
        'mediumInt' => 2,
        'integer' => 3,
        'bigInt' => 4,
        'decimal' => 1.24,
        'varchar' => 'to je test',
        'text' => 'text text text',
        'timestamp' => date('Y-m-d H:i:s', time()),
        //'timestamp' => time(),
        'time' => '13:01:00',
        'date' => '1992-01-20',
        'datetime' => '1992-01-20 13:01:00',
        'year' => '2007',
        'enum' => 'a',
        'set' => 'a,b'
    ],
    2 => [
        'id_main' => 2,
        'boolean' => 0,
        'smallInt' => 2,
        'mediumInt' => 3,
        'integer' => 4,
        'bigInt' => 5,
        'decimal' => 4.21,
        'varchar' => 'ddddjjjjiiiiuuuu',
        'text' => "a\nb\nc\nd",
        'timestamp' => date('Y-m-d H:i:s', time() - 100),
        'time' => '01:01:00',
        'date' => '2017-01-21',
        'datetime' => '2010-01-20 14:01:00',
        'year' => '2000',
        'enum' => 'b',
        'set' => 'b,c'
    ]
    ]
);

Test::data(
    'sdata', [
    1 => [
        'secondary_id' => 1,
        'id_main' => 1,
        'data' => 'a',
    ],
    2 => [
        'secondary_id' => 2,
        'id_main' => 1,
        'data' => 'c',
    ],
    3 => [
        'secondary_id' => 3,
        'id_main' => 2,
        'data' => 'b',
    ],
    ]
);
