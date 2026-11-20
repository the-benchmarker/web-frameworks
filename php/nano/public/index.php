<?php

use laylatichy\nano\core\request\Request;
use laylatichy\nano\core\response\Response;

require_once 'vendor/autoload.php';

useNano();

useRouter()->get('/', fn (Request $request): Response => useResponse()
    ->withText('')
);

useRouter()->get('/user/{id}', fn (Request $request, string $id): Response => useResponse()
    ->withText($id)
);

useRouter()->post('/user', fn (Request $request): Response => useResponse()
    ->withText('')
);

useNano()->start();
