<?php

namespace App\Http\Middleware;

use Closure;
use MacropaySolutions\Kernel\Contracts\Auth\Factory as Auth;

class Authenticate
{
    /**
     * The authentication guard factory instance.
     *
     * @var \MacropaySolutions\Kernel\Contracts\Auth\Factory
     */
    protected $auth;

    /**
     * Create a new middleware instance.
     */
    public function __construct(Auth $auth)
    {
        $this->auth = $auth;
    }

    /**
     * Handle an incoming request.
     *
     * @param  \MacropaySolutions\Kernel\Http\Request  $request
     * @param  \Closure  $next
     * @param  string|null  $guard
     */
    public function handle($request, Closure $next, $guard = null): mixed
    {
        if ($this->auth->guard($guard)->guest()) {
            return response('Unauthorized.', 401);
        }

        $this->auth->shouldUse($guard);

        return $next($request);
    }
}
