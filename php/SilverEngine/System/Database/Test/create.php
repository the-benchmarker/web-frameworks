<?php

namespace Silver\Test\Database;

use Silver\Database\Query;

Test::define(Query::drop(Test::data('weird'))->ifExists());
Test::define(Query::drop('secondary')->ifExists());
Test::define(Query::drop('main')->ifExists());
Test::define(
    Query::create(
        Test::data('weird'), function ($q) {
            $q->integer('id')->primary()->autoincrement();
        }
    )
);
Test::define(
    Query::create(
        'main', function ($q) {
            $q->integer('id_main')->primary()->autoincrement();
        
            $q->boolean('boolean');
            $q->smallInt("smallInt");

            $q->mediumInt("mediumInt");

            $q->integer("integer");
            $q->bigInt("bigInt");
            $q->decimal("decimal", 6, 5); //
            $q->varchar("varchar", 255);
            $q->text("text");
            $q->timestamp("timestamp");
            $q->time("time");
            $q->date("date");
            $q->datetime("datetime");
            $q->year("year");

            $q->enum('enum', 'a', 'b', 'c');
            $q->set('set', 'a', 'b', 'c');
        }
    )
);
Test::define(
    Query::create(
        'secondary', function ($q) {
            $q->integer('secondary_id')->primary()->autoincrement();
            $q->text('data');
            $q->integer('id_main')->nullable()->references('main.id_main'); // FIXME: references must be at end of definitions
        }
    )
);
