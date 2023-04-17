<?php

namespace Application;

use Application\Resources\ApplicationResource;
use Application\Resources\UserIDResource;
use Application\Resources\UserResource;
use Cache\Adapter\PHPArray\ArrayCachePool;
use Hamlet\Http\Applications\AbstractApplication;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\HttpResource;
use Hamlet\Http\Resources\NotFoundResource;
use Psr\Cache\CacheItemPoolInterface;

class Application extends AbstractApplication
{
    /** @var CacheItemPoolInterface */
    private $cache;

    public function __construct()
    {
        $this->cache = new ArrayCachePool();
    }

    public function findResource(Request $request): HttpResource
    {
        if ($request->getPath() == '/user') {
            return new UserResource();
        } elseif ($request->getPath() == '/') {
            return new ApplicationResource();
        } elseif ($parts = $request->pathMatchesPattern('/user/{id}')) {
            return new UserIDResource($parts['id']);
        }
        return new NotFoundResource();
    }

    protected function getCache(Request $request): CacheItemPoolInterface
    {
        return $this->cache;
    }
}
