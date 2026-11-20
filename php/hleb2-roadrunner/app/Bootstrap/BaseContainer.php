<?php

namespace App\Bootstrap;

use Hleb\Constructor\Containers\CoreContainer;

final class BaseContainer extends CoreContainer implements ContainerInterface
{
    #[\Override]
    final public function get(string $id): mixed
    {
        return parent::get($id);
    }
}
