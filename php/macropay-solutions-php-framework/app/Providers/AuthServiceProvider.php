<?php

namespace App\Providers;

use App\Models\User;
use MacropaySolutions\Kernel\Auth\AuthManager;
use MacropaySolutions\Kernel\Container\Container;
use MacropaySolutions\Kernel\Contracts\Support\DeferrableProvider;
use MacropaySolutions\Kernel\Http\Request;
use MacropaySolutions\Kernel\Support\ServiceProvider;

class AuthServiceProvider extends ServiceProvider implements DeferrableProvider

{
    /**
     * Register any application services.
     */
    public function register(): void
    {
        // instances/singletons run their resolving events only once
        $this->app->afterResolving('auth', [$this, 'configureAuthGuard']);
    }

    /**
     * Configure the API guard for the application.
     */
    public function configureAuthGuard(AuthManager $auth, Container $app): void
    {
        $auth->viaRequest('api', static function (Request $request): ?User {
//             $apiToken = $request->input('api_token');
//
//             return (\is_string($apiToken) && $apiToken !== '') ?
//                 User::query()->where('api_token', \hash('sha256', $apiToken))->first() :
//                 null;
//            //or
//            $id = $request->header('X-API-ID');
//            $pass = $request->header('X-API-PASS');
//
//            if (!\is_string($id) || $id === '' || !\is_string($pass) || $pass === '') {
//                return null;
//            }
//
//            $user = User::query()->find($id) ?? new User();
//
//            if (
//                \password_verify($pass, $user->getAttributeValue('pass') ??
//                    '$2y$10$TKh8H1.PfQx37YgCzwiKb.KjNyWgaHb9cbcoQgdIVFlYg7B77UdFm') // hash for `secret`
//                && $user->exists
//            ) {
//                return $user;
//            }
//
//            return null;

            return null;
        });
    }
}
