<?php

declare(strict_types=1);

namespace Migration;

use Cycle\Migrations\Migration;

class OrmDefault6b37c029f83df5d5b93667bad89b5fc9 extends Migration
{
    protected const DATABASE = 'default';

    public function up(): void
    {
        $this
            ->table('users')
            ->addColumn('id', 'primary', ['nullable' => false, 'default' => null])
            ->addColumn('username', 'string', ['nullable' => false, 'default' => null])
            ->addColumn('email', 'string', ['nullable' => false, 'default' => null])
            ->setPrimaryKeys(['id'])
            ->create();
    }

    public function down(): void
    {
        $this->table('users')->drop();
    }
}
