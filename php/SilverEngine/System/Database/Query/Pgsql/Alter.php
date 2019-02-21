<?php

namespace Silver\Database\Query\Pgsql;

use Silver\Database\Query\Alter as P;

class Alter extends P
{
    protected static function compileChange($c, $add) 
    {
        list($old, $newdef) = $c;
        $newname = $newdef->getName();
        $add("RENAME COLUMN $old TO $newname");
        self::compileModify($newdef, $add);
    }

    // Barely supported
    protected static function compileModify($c, $add) 
    {
        $name = $c->getName();
        $type = $c->getType();
        $add("ALTER $name TYPE $type");

        if (($def = $c->getDefault()) !== null) {
            // FIXMEEEeeeeeeeeeeeeeeeeeeeeeeee: use needed???
            $add("ALTER $name SET DEFAULT " . Literal::ensure($def));
        } else {
            $add("ALTER $name DROP DEFAULT");
        }

        if ($c->getNullable()) {
            $add("ALTER $name SET NOT NULL");
        } else {
            $add("ALTER $name DROP NOT NULL");
        }
    }
}