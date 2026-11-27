<?php

namespace App;

use FastRoute\Dispatcher;
use MacropaySolutions\CrufdWizard\Providers\CrufdProvider;
use MacropaySolutions\Framework\Console\ConsoleServiceProvider;
use MacropaySolutions\Kernel\Events\EventServiceProvider;
use MacropaySolutions\Kernel\Http\JsonResponse;
use MacropaySolutions\Kernel\Mail\MailServiceProvider;
use Psr\Log\LoggerInterface;

class Application extends \MacropaySolutions\Framework\Application
{
    /**
     * Set to true if you need/want
     */
    public const DEFAULT_PARAMETER_TAKES_PRECEDENCE_WHEN_AUTOWIRING = false;

    /**
     * To avoid calls to
     * @see \MacropaySolutions\Framework\Concerns\RoutesRequests::middleware()
     * Make sure this list has UNIQUE values!!!
     */
    protected array $middleware = [
        // \App\Http\Middleware\TrustProxies::class,
    ];

    /**
     * Global middlewares list that are applied ONLY after the route is found
     * Make sure this list has UNIQUE values!!!
     */
    protected array $foundRouteMiddleware = [
        //
    ];

    /**
     * To avoid calls to
     * @see \MacropaySolutions\Framework\Concerns\RoutesRequests::routeMiddleware()
     * Note that you can use the middleware FQN on a route without declaring its alias here!
     */
    protected array $routeMiddleware = [
        //'auth' => \App\Http\Middleware\Authenticate::class,
    ];

    /**
     * Pre-compiled bindings array.
     * Replaces closure-based bindings to be loaded directly into memory by OPcache.
     * @see Application::aliases to handle alias changes if impacted by additions here
     * @see Application::$abstractAliases to handle alias changes if impacted by additions here
     * @var array[]
     */
    protected array $bindings = [
        'config' => [
            'concrete' => [self::class, 'getConfig'],
            'shared' => true
        ],
        'composer' => [
            'concrete' => [self::class, 'getComposer'],
            'shared' => true
        ],
        'files' => [
            'concrete' => [self::class, 'getFiles'],
            'shared' => true
        ],
        LoggerInterface::class => [
            'concrete' => [self::class, 'getLogger'],
            'shared' => true
        ],
        'router' => [
            'concrete' => [self::class, 'getRouter'],
            'shared' => true
        ],
        \Psr\Http\Message\ServerRequestInterface::class => [
            'concrete' => [self::class, 'getPsrRequest'],
            'shared' => true
        ],
        \Psr\Http\Message\ResponseInterface::class => [
            'concrete' => [self::class, 'getPsrResponse'],
            'shared' => true
        ],
        'url' => [
            'concrete' => [self::class, 'getUrlGenerator'],
            'shared' => true
        ],
        \MacropaySolutions\Kernel\Contracts\Auth\Access\Gate::class => [
            'concrete' => [self::class, 'getGate'],
            'shared' => true
        ],

        // App Overrides & Additions
        \MacropaySolutions\Kernel\Contracts\Debug\ExceptionHandler::class => [
            'concrete' => [\App\Factories\ContainerBindingsFactory::class, 'createExceptionHandler'],
            'shared' => true,
        ],
        \MacropaySolutions\Kernel\Contracts\Console\Kernel::class => [
            'concrete' => [\App\Factories\ContainerBindingsFactory::class, 'createConsoleKernel'],
            'shared' => true,
        ],
        JsonResponse::class => [
            'concrete' => [CrufdProvider::class, 'createJsonResponse'],
            'shared' => false,
        ],
    ];

    /**
     * The available container bindings and their respective load methods.
     * Uncomment the needed bindings and also, remove their module from composer.json autoload exclude-from-classmap
     * @see \App\Application::prepareForConsoleCommand also
     *
     * @var array
     */
    protected array $availableBindings = [
        // --- AUTH ---
        'auth' => 'registerAuthBindings',
        'auth.driver' => 'registerAuthBindings',
        \MacropaySolutions\Kernel\Auth\AuthManager::class => 'registerAuthBindings',
        \MacropaySolutions\Kernel\Contracts\Auth\Guard::class => 'registerAuthBindings',
        \MacropaySolutions\Kernel\Contracts\Auth\Access\Gate::class => 'registerGateAuthBindings',

        // --- BROADCASTING ---
        \MacropaySolutions\Kernel\Contracts\Broadcasting\Broadcaster::class => 'registerBroadcastingBindings',
        \MacropaySolutions\Kernel\Contracts\Broadcasting\Factory::class => 'registerBroadcastingBindings',
        \MacropaySolutions\Kernel\Broadcasting\BroadcastManager::class => 'registerBroadcastingBindings',

        \MacropaySolutions\Kernel\Contracts\Bus\Dispatcher::class => 'registerBusBindings',
        \MacropaySolutions\Kernel\Contracts\Bus\QueueingDispatcher::class => 'registerBusBindings',
        \MacropaySolutions\Kernel\Bus\Dispatcher::class => 'registerBusBindings',
        'cache' => 'registerCacheBindings',
        'cache.store' => 'registerCacheBindings',
        'cache.psr6' => 'registerCacheBindings',
        'memcached.connector' => 'registerCacheBindings',
        \MacropaySolutions\Kernel\Cache\RateLimiter::class => 'registerCacheBindings',
        \MacropaySolutions\Kernel\Contracts\Cache\Factory::class => 'registerCacheBindings',
        \MacropaySolutions\Kernel\Contracts\Cache\Repository::class => 'registerCacheBindings',
        'db' => 'registerDatabaseBindings',
        'filesystem' => 'registerFilesystemBindings',
        'filesystem.cloud' => 'registerFilesystemBindings',
        'filesystem.disk' => 'registerFilesystemBindings',
        \MacropaySolutions\Kernel\Contracts\Filesystem\Cloud::class => 'registerFilesystemBindings',
        \MacropaySolutions\Kernel\Contracts\Filesystem\Filesystem::class => 'registerFilesystemBindings',
        \MacropaySolutions\Kernel\Contracts\Filesystem\Factory::class => 'registerFilesystemBindings',

        // --- ENCRYPTION ---
        'encrypter' => 'registerEncrypterBindings',
        \MacropaySolutions\Kernel\Contracts\Encryption\Encrypter::class => 'registerEncrypterBindings',

        'events' => 'registerEventBindings',
        \MacropaySolutions\Kernel\Contracts\Events\Dispatcher::class => 'registerEventBindings',

        // --- HASH ---
        'hash' => 'registerHashBindings',
        \MacropaySolutions\Kernel\Contracts\Hashing\Hasher::class => 'registerHashBindings',

        'queue' => 'registerQueueBindings',
        'queue.connection' => 'registerQueueBindings',
        'queue.worker' => 'registerQueueBindings',
        'queue.listener' => 'registerQueueBindings',
        'queue.failer' => 'registerQueueBindings',
        \MacropaySolutions\Kernel\Contracts\Queue\Factory::class => 'registerQueueBindings',
        \MacropaySolutions\Kernel\Contracts\Queue\Queue::class => 'registerQueueBindings',

        'translator' => 'registerTranslationBindings',
        'validator' => 'registerValidatorBindings',
        \MacropaySolutions\Kernel\Contracts\Validation\Factory::class => 'registerValidatorBindings',

        // --- VIEW ---
        'view' => 'registerViewBindings',
        \MacropaySolutions\Kernel\Contracts\View\Factory::class => 'registerViewBindings',
        'view.finder' => 'registerViewBindings',
        'template.compiler' => 'registerViewBindings',
        'view.engine.resolver' => 'registerViewBindings',
        \MacropaySolutions\Kernel\View\Engines\EngineResolver::class => 'registerViewBindings',

        // --- SESSION & COOKIE ---
        'session' => 'registerSessionBindings',
        'session.store' => 'registerSessionBindings',
        \MacropaySolutions\Kernel\Session\Middleware\StartSession::class => 'registerSessionBindings',
        'cookie' => 'registerCookieBindings',

//        // --- MAIL ---
//        'mailer' => 'registerMailBindings',
//        'mail.manager' => 'registerMailBindings',
//        \MacropaySolutions\Kernel\Mail\Markdown::class => 'registerMailBindings',

        // --- NOTIFICATIONS ---
        \MacropaySolutions\Kernel\Contracts\Notifications\Dispatcher::class => 'registerNotificationBindings',
        \MacropaySolutions\Kernel\Contracts\Notifications\Factory::class => 'registerNotificationBindings',
        \MacropaySolutions\Kernel\Notifications\ChannelManager::class => 'registerNotificationBindings',
    ];

    protected array $abstractAliases = [
        'app' => [
            \App\Application::class,
            \MacropaySolutions\Framework\Application::class,
            \MacropaySolutions\Kernel\Contracts\Foundation\Application::class,
            \MacropaySolutions\Kernel\Container\Container::class,
            \MacropaySolutions\Kernel\Contracts\Container\Container::class,
        ],
        'auth' => [
            \MacropaySolutions\Kernel\Contracts\Auth\Factory::class,
            \MacropaySolutions\Kernel\Auth\AuthManager::class,
        ],
        'auth.driver' => [
            \MacropaySolutions\Kernel\Contracts\Auth\Guard::class,
        ],
        'cache' => [
            \MacropaySolutions\Kernel\Contracts\Cache\Factory::class,
            \MacropaySolutions\Kernel\Cache\CacheManager::class,
        ],
        'cache.store' => [
            \MacropaySolutions\Kernel\Contracts\Cache\Repository::class,
        ],
        'config' => [
            \MacropaySolutions\Kernel\Contracts\Config\Repository::class,
            \MacropaySolutions\Kernel\Config\Repository::class,
        ],
        'db' => [
            \MacropaySolutions\Kernel\Database\ConnectionResolverInterface::class,
            \MacropaySolutions\Kernel\Database\DatabaseManager::class,
        ],
        'encrypter' => [
            \MacropaySolutions\Kernel\Contracts\Encryption\Encrypter::class,
            \MacropaySolutions\Kernel\Encryption\Encrypter::class,
        ],
        'events' => [
            \MacropaySolutions\Kernel\Contracts\Events\Dispatcher::class,
            \MacropaySolutions\Kernel\Events\Dispatcher::class,
        ],
        'filesystem' => [
            \MacropaySolutions\Kernel\Contracts\Filesystem\Factory::class,
            \MacropaySolutions\Kernel\Filesystem\FilesystemManager::class,
        ],
        'filesystem.disk' => [
            \MacropaySolutions\Kernel\Contracts\Filesystem\Filesystem::class,
        ],
        'filesystem.cloud' => [
            \MacropaySolutions\Kernel\Contracts\Filesystem\Cloud::class,
        ],
        'hash' => [
            \MacropaySolutions\Kernel\Contracts\Hashing\Hasher::class,
            \MacropaySolutions\Kernel\Hashing\HashManager::class,
        ],
        // quirk: Key is the Interface for 'log'
        \Psr\Log\LoggerInterface::class => [
            'log',
        ],
        'queue' => [
            \MacropaySolutions\Kernel\Contracts\Queue\Factory::class,
            \MacropaySolutions\Kernel\Queue\QueueManager::class,
        ],
        'queue.connection' => [
            \MacropaySolutions\Kernel\Contracts\Queue\Queue::class,
        ],
        'redis' => [
            \MacropaySolutions\Kernel\Contracts\Redis\Factory::class,
            \MacropaySolutions\Kernel\Redis\RedisManager::class,
        ],
        'redis.connection' => [
            \MacropaySolutions\Kernel\Redis\Connections\Connection::class,
            \MacropaySolutions\Kernel\Contracts\Redis\Connection::class,
        ],
        // quirk: Key is the Request class
        \MacropaySolutions\Kernel\Http\Request::class => [
            'request',
            \App\Request::class,
            \MacropaySolutions\Framework\Http\Request::class,
        ],
        'router' => [
            \App\Router::class,
            \MacropaySolutions\Framework\Routing\Router::class,
        ],
        'translator' => [
            \MacropaySolutions\Kernel\Contracts\Translation\Translator::class,
            \MacropaySolutions\Kernel\Translation\Translator::class,
        ],
        'url' => [
            \MacropaySolutions\Framework\Routing\UrlGenerator::class,
        ],
        'validator' => [
            \MacropaySolutions\Kernel\Contracts\Validation\Factory::class,
            \MacropaySolutions\Kernel\Validation\Factory::class,
        ],
        'view' => [
            \MacropaySolutions\Kernel\Contracts\View\Factory::class,
            \MacropaySolutions\Kernel\View\Factory::class,
        ],
        'template.compiler' => [
            \MacropaySolutions\Kernel\View\Compilers\TemplateCompiler::class,
        ],
        'view.engine.resolver' => [
            \MacropaySolutions\Kernel\View\Engines\EngineResolver::class,
        ],
        'session' => [
            \MacropaySolutions\Kernel\Session\SessionManager::class,
        ],
        'session.store' => [
            \MacropaySolutions\Kernel\Session\Store::class,
            \MacropaySolutions\Kernel\Contracts\Session\Session::class,
        ],
        'cookie' => [
            \MacropaySolutions\Kernel\Cookie\CookieJar::class,
            \MacropaySolutions\Kernel\Contracts\Cookie\Factory::class,
            \MacropaySolutions\Kernel\Contracts\Cookie\QueueingFactory::class,
        ],

//        // --- MAIL ---
//        'mail.manager' => [
//        \MacropaySolutions\Kernel\Mail\MailManager::class,
//        \MacropaySolutions\Kernel\Contracts\Mail\Factory::class,
//        ],
//        'mailer' => [
//        \MacropaySolutions\Kernel\Mail\Mailer::class,
//        \MacropaySolutions\Kernel\Contracts\Mail\Mailer::class,
//        \MacropaySolutions\Kernel\Contracts\Mail\MailQueue::class,
//        ],

        'files' => [
            \MacropaySolutions\Kernel\Filesystem\Filesystem::class,
        ],
        \MacropaySolutions\Kernel\Broadcasting\BroadcastManager::class => [
            \MacropaySolutions\Kernel\Contracts\Broadcasting\Factory::class,
        ],
        \MacropaySolutions\Kernel\Bus\Dispatcher::class => [
            \MacropaySolutions\Kernel\Contracts\Bus\Dispatcher::class,
            \MacropaySolutions\Kernel\Contracts\Bus\QueueingDispatcher::class,
        ],
        \MacropaySolutions\Kernel\Notifications\ChannelManager::class => [
            \MacropaySolutions\Kernel\Contracts\Notifications\Dispatcher::class,
            \MacropaySolutions\Kernel\Contracts\Notifications\Factory::class,
        ],
    ];

    protected array $aliases = [
        \App\Application::class => 'app',
        \MacropaySolutions\Framework\Application::class => 'app',
        \MacropaySolutions\Kernel\Contracts\Foundation\Application::class => 'app',
        \MacropaySolutions\Kernel\Contracts\Auth\Factory::class => 'auth',
        \MacropaySolutions\Kernel\Contracts\Auth\Guard::class => 'auth.driver',
        \MacropaySolutions\Kernel\Auth\AuthManager::class => 'auth',
        \MacropaySolutions\Kernel\Contracts\Cache\Factory::class => 'cache',
        \MacropaySolutions\Kernel\Contracts\Cache\Repository::class => 'cache.store',
        \MacropaySolutions\Kernel\Contracts\Config\Repository::class => 'config',
        \MacropaySolutions\Kernel\Config\Repository::class => 'config',
        \MacropaySolutions\Kernel\Container\Container::class => 'app',
        \MacropaySolutions\Kernel\Contracts\Container\Container::class => 'app',
        \MacropaySolutions\Kernel\Database\ConnectionResolverInterface::class => 'db',
        \MacropaySolutions\Kernel\Database\DatabaseManager::class => 'db',
        \MacropaySolutions\Kernel\Contracts\Encryption\Encrypter::class => 'encrypter',
        \MacropaySolutions\Kernel\Encryption\Encrypter::class => 'encrypter',
        \MacropaySolutions\Kernel\Contracts\Events\Dispatcher::class => 'events',
        \MacropaySolutions\Kernel\Contracts\Filesystem\Factory::class => 'filesystem',
        \MacropaySolutions\Kernel\Contracts\Filesystem\Filesystem::class => 'filesystem.disk',
        \MacropaySolutions\Kernel\Contracts\Filesystem\Cloud::class => 'filesystem.cloud',
        \MacropaySolutions\Kernel\Contracts\Hashing\Hasher::class => 'hash',
        \MacropaySolutions\Kernel\Hashing\HashManager::class => 'hash',
        'log' => \Psr\Log\LoggerInterface::class,
        \MacropaySolutions\Kernel\Contracts\Queue\Factory::class => 'queue',
        \MacropaySolutions\Kernel\Contracts\Queue\Queue::class => 'queue.connection',
        \MacropaySolutions\Kernel\Queue\QueueManager::class => 'queue',
        \MacropaySolutions\Kernel\Redis\RedisManager::class => 'redis',
        \MacropaySolutions\Kernel\Contracts\Redis\Factory::class => 'redis',
        \MacropaySolutions\Kernel\Redis\Connections\Connection::class => 'redis.connection',
        \MacropaySolutions\Kernel\Contracts\Redis\Connection::class => 'redis.connection',
        'request' => \MacropaySolutions\Kernel\Http\Request::class,
        \App\Request::class => \MacropaySolutions\Kernel\Http\Request::class,
        \MacropaySolutions\Framework\Http\Request::class => \MacropaySolutions\Kernel\Http\Request::class,
        \App\Router::class => 'router',
        \MacropaySolutions\Framework\Routing\Router::class => 'router',
        \MacropaySolutions\Kernel\Contracts\Translation\Translator::class => 'translator',
        \MacropaySolutions\Framework\Routing\UrlGenerator::class => 'url',
        \MacropaySolutions\Kernel\Contracts\Validation\Factory::class => 'validator',
        \MacropaySolutions\Kernel\Contracts\View\Factory::class => 'view',
        \MacropaySolutions\Kernel\Session\SessionManager::class => 'session',
        \MacropaySolutions\Kernel\Session\Store::class => 'session.store',
        \MacropaySolutions\Kernel\Contracts\Session\Session::class => 'session.store',
        \MacropaySolutions\Kernel\Cookie\CookieJar::class => 'cookie',
        \MacropaySolutions\Kernel\Contracts\Cookie\Factory::class => 'cookie',
        \MacropaySolutions\Kernel\Contracts\Cookie\QueueingFactory::class => 'cookie',

//        // --- MAIL ---
//        \MacropaySolutions\Kernel\Mail\MailManager::class => 'mail.manager',
//        \MacropaySolutions\Kernel\Contracts\Mail\Factory::class => 'mail.manager',
//        \MacropaySolutions\Kernel\Mail\Mailer::class => 'mailer',
//        \MacropaySolutions\Kernel\Contracts\Mail\Mailer::class => 'mailer',
//        \MacropaySolutions\Kernel\Contracts\Mail\MailQueue::class => 'mailer',

        \MacropaySolutions\Kernel\Cache\CacheManager::class => 'cache',
        \MacropaySolutions\Kernel\Events\Dispatcher::class => 'events',
        \MacropaySolutions\Kernel\Filesystem\FilesystemManager::class => 'filesystem',
        \MacropaySolutions\Kernel\Filesystem\Filesystem::class => 'files',
        \MacropaySolutions\Kernel\Translation\Translator::class => 'translator',
        \MacropaySolutions\Kernel\Validation\Factory::class => 'validator',
        \MacropaySolutions\Kernel\View\Factory::class => 'view',
        \MacropaySolutions\Kernel\View\Compilers\TemplateCompiler::class => 'template.compiler',
        \MacropaySolutions\Kernel\View\Engines\EngineResolver::class => 'view.engine.resolver',
        \MacropaySolutions\Kernel\Contracts\Broadcasting\Factory::class =>
            \MacropaySolutions\Kernel\Broadcasting\BroadcastManager::class,
        \MacropaySolutions\Kernel\Contracts\Bus\Dispatcher::class => \MacropaySolutions\Kernel\Bus\Dispatcher::class,
        \MacropaySolutions\Kernel\Contracts\Bus\QueueingDispatcher::class =>
            \MacropaySolutions\Kernel\Bus\Dispatcher::class,
        \MacropaySolutions\Kernel\Contracts\Notifications\Dispatcher::class =>
            \MacropaySolutions\Kernel\Notifications\ChannelManager::class,
        \MacropaySolutions\Kernel\Contracts\Notifications\Factory::class =>
            \MacropaySolutions\Kernel\Notifications\ChannelManager::class,
    ];

    protected ConsoleServiceProvider $consoleProvider;

    protected function registerDevConsoleProviders(): void
    {
        parent::registerDevConsoleProviders();
        $this->register(\MacropaySolutions\CrufdWizardGenerator\CrufdWizardGeneratorServiceProvider::class);
    }

    protected function registerAuthBindings()
    {
        parent::registerAuthBindings();
        $this->register(\App\Providers\AuthServiceProvider::class);
    }

    protected function registerEventBindings()
    {
        $this->register(EventServiceProvider::class);
        $this->register(\App\Providers\EventServiceProvider::class);
    }

    /**
     * @inheritDoc
     */
    public function bootstrapRouter(): void
    {
        $this->router = new Router($this);
    }

    /**
     * @inheritdoc
     */
    protected function createDispatcher(): Dispatcher
    {
        if (isset($this->dispatcher)) {
            return $this->dispatcher;
        }

        $closure = function (\FastRoute\RouteCollector $r): void {
            foreach ($this->router->getComplexRoutes() as $route) {
                $r->addRoute($route['method'], $route['uri'], $route['action']);
            }
        };

        if (!$this->routesAreCached()) {
            return \FastRoute\simpleDispatcher($closure);
        }

        return \FastRoute\cachedDispatcher($closure, [
            'cacheFile' => $this->getCachedFastRoutesPath()
        ]);
    }

//    /**
//     * Uncomment to use \App\FormRequest
//     * @inheritdoc
//     */
//    protected function fireResolvingCallbacks($abstract, $object)
//    {
//        $this->fireCallbackArray($object, $this->globalResolvingCallbacks);
//
//        if ($object instanceof \MacropaySolutions\Kernel\Http\FormRequest) {
//        \MacropaySolutions\Kernel\Http\FormRequest::createFrom($this->make('request'), $object);
//
//        $object->setContainer($this);
//        }
//
//        $this->fireCallbackArray(
//        $object,
//        $this->getResolvingCallbacksForType($abstract, $object)
//        );
//
//        if ($object instanceof \MacropaySolutions\Kernel\Contracts\Validation\ValidatesWhenResolved) {
//        $object->validateResolved();
//        }
//
//        $this->fireAfterResolvingCallbacks($abstract, $object);
//    }

    protected function registerMailBindings(): void
    {
        $this->configure('mail');
        $this->register(MailServiceProvider::class);
    }
}
