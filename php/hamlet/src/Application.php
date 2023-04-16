<?php

namespace Application;

use Application\Resources\ApplicationResource;
use Application\Resources\UserResource;
use Application\Resources\UserIDResource;
use Cache\Adapter\PHPArray\ArrayCachePool;
use Hamlet\Http\Applications\AbstractApplication;
use Hamlet\Http\Requests\Request;
use Hamlet\Http\Resources\HttpResource;
use Hamlet\Http\Resources\NotFoundResource;
use Hamlet\Http\Responses\Response;
use Hamlet\Http\Responses\ServerErrorResponse;
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
        $path = $request->getPath();
        $parts = \explode(DIRECTORY_SEPARATOR, $path);

        if ($path === '/user' && $request->getMethod() === 'POST') {
            return new UserResource();
        }
        if ($request->getMethod() === 'GET') {
            if ($path === '/') {
                return new ApplicationResource();
            } elseif (count($parts) === 3 && $parts[1] === 'user') {
                return new UserIDResource($parts[2]);
            }
        }
        return new NotFoundResource();
    }

    protected function getCache(Request $request): CacheItemPoolInterface
    {
        return $this->cache;
    }
}
