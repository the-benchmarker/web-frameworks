<?php

namespace Silver\Test\Database;

use Silver\Database\Query;

Test::define(
    Query::drop('test')->ifExists(),
    null,
    'drop table test'
);
Test::define(
    Query::create(
        'test', function ($q) {
            $q->integer('id')->primary()->autoincrement();
            $q->varchar('data', 10);
        }
    ),
    null,
    'create table test'
);

Test::define(
    Query::insert('test', ['data' => '0123456789']),
    null,
    'insert data'
);

Test::define(
    Query::alter(
        'test', function ($q) {
            $q->addColumn('num', 'integer')->nullable();
        }
    ),
    null,
    'alter - add new column'
);

Test::define(
    Query::select()->from('test'),
    ['id' => 1, 'data' => '0123456789', 'num' => null],
    null,
    'alter - check table'
);

Test::on(
    ['pgsql', 'mysqlite'], function () {
        Test::define(
            Query::alter(
                'test', function ($q) {
                    $q->modifyColumn('data', 'text');
                }
            ),
            null,
            'varchar -> text'
        );

        Test::define(
            Query::update('test')->set('data', 'aaaaaaaaaabbbbbbbbbb'),
            null,
            'update data'
        );

        Test::define(
            Query::select()->from('test'),
            ['id' => 1, 'data' => 'aaaaaaaaaabbbbbbbbbb', 'num' => null],
            'data-check'
        );
    }
);
