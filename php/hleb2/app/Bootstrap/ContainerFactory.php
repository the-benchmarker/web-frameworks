<?php

namespace App\Bootstrap;

use Hleb\Constructor\Containers\BaseContainerFactory;

final class ContainerFactory extends BaseContainerFactory
{
    #[\Override]
    public static function rollback(): void
    {
    }
}
