from masonite.providers import (
    RouteProvider,
    FrameworkProvider,
    ViewProvider,
    ExceptionProvider,
    SessionProvider,
    QueueProvider,
    StorageProvider,
    AuthenticationProvider,
    AuthorizationProvider,
    ORMProvider,
    EventProvider,
)


PROVIDERS = [
    FrameworkProvider,
    RouteProvider,
    ViewProvider,
    EventProvider,
    ExceptionProvider,
    SessionProvider,
    QueueProvider,
    StorageProvider,
    AuthenticationProvider,
    AuthorizationProvider,
    ORMProvider,
]
