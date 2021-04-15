<?php

namespace Application\Resources;

use Hamlet\Http\Entities\PlainTextEntity;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\HttpResource;
use Hamlet\Http\Responses\MethodNotAllowedResponse;
use Hamlet\Http\Responses\Response;
use Hamlet\Http\Responses\SimpleOKResponse;

class UserIDResource implements HttpResource
{
    /**
     * @var string
     */
    private $_id;

    public function __construct(string $id)
    {
        $this->_id = $id;
    }

    public function getResponse(Request $request): Response
    {
        if ($request->getMethod() == 'GET') {
            $entity = new PlainTextEntity($this->_id);
            return new SimpleOKResponse($entity);
        } else {
            return new MethodNotAllowedResponse('GET');
        }
    }
}
