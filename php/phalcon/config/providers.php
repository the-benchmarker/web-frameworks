<?php
declare(strict_types=1);

use Application\Providers\ConfigProvider;
use Application\Providers\DispatcherProvider;
use Application\Providers\RouterProvider;
use Application\Providers\UrlProvider;
use Application\Providers\ViewProvider;

return [
    ConfigProvider::class,
    DispatcherProvider::class,
    RouterProvider::class,
    UrlProvider::class,
    ViewProvider::class,
];
