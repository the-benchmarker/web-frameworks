<?php

namespace Silver\Test\Database;

use Silver\Database\Query;
use Silver\Database\Parts\Column;
use Silver\Database\Parts\Fn;

$weird = Test::data('weird');
$data = Test::data('data');
$sdata = Test::data('sdata');

function only($arr, ...$keys) 
{
    $r = [];
    foreach ($keys as $key) {
        $r[$key] = $arr[$key];
    }
    return $r;
}

Test::define(Query::select()->from($weird), ['id' => 24], 'weird');
Test::define(
    Query::select()->from('main')->where('id_main', 2),
    $data[2],
    'select *'
);
Test::define(
    Query::select('boolean', 'date', 'bigInt')->from('main')->where('id_main', 2),
    only($data[2], 'boolean', 'date', 'bigInt'),
    'select X'
);
Test::define(
    Query::select()
    ->from('main')
    ->from('secondary', 'main.id_main', 'secondary.id_main')
    ->where('main.id_main', 1)
    ->offset(1),
    array_merge($data[1], $sdata[2]),
    'select from from'
);
Test::define(
    Query::select('m.enum', 's.data')
    ->from('main m')
    ->leftJoin('secondary s', ['s.id_main', 'm.id_main'])
    ->where('s.secondary_id', 3),
    only(array_merge($data[2], $sdata[3]), 'enum', 'data'),
    'select alias join'
);
Test::define(
    Query::count('c')
    ->from('main m')
    ->leftJoin('secondary s', 'id_main'),
    ['c' => 3],
    'count1'
);
Test::define(
    Query::select(
        // TODO: simplify this
        Column::ensure(
            [
            null,
            Fn::groupConcat('s.secondary_id'),
            'ids'
            ]
        ),
        Column::ensure(
            [
            null,
            Fn::groupConcat('s.data', '-'),
            'dt'
            ]
        )
    )
    ->from('secondary s')
    ->groupBy('id_main')
    // postgresql doesn't support references to select columns in having
    // ->having('dt', 'a,c'),
    ->having(Fn::groupConcat('s.data', ':'), 'a:c'),
    ['ids' => '1,2', 'dt' => 'a-c'],
    'having'
);
Test::define(
    Query::count()
    ->from(
        Query::select('id_main id', 'text text')
           ->from('main')
           ->union(Query::select('secondary_id id', 'data text')->from('secondary')),
        'name'
    ),
    ['count' => 5],
    'union'
);
