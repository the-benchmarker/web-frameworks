<?php

namespace Silver\Test\Database;

use Silver\Database\Query;

$weird = Test::data('weird');
$data = Test::data('data');
$sdata = Test::data('sdata');

Test::define(Query::insert($weird, [24]));
Test::define(Query::insert('main', array_values($data)));
Test::define(Query::insert('secondary', array_values($sdata)));
