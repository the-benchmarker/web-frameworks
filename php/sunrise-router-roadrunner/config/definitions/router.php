<?php

declare(strict_types=1);

use function DI\add;
use function DI\string;

return [
    'router.descriptor_loader.resources' => add([
        string('{app.root}/src/Controller'),
    ]),
];
