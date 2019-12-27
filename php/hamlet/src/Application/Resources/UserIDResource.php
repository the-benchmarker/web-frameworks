<?php

namespace Application\Resources;

use Hamlet\Http\Entities\PlainTextEntity;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\HttpResource;
use Hamlet\Http\Responses\Response;
use Hamlet\Http\Responses\SimpleOKResponse;

class UserIDResource implements HttpResource
{
    private $_id = null;

    public function __construct(string $id)
    {
        $this->_id = $id;
    }

    public function getResponse(Request $request): Response
    {
        $entity = new PlainTextEntity($this->_id);
        return new SimpleOKResponse($entity);
    }
}
