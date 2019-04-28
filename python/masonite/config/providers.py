"""Providers Configuration File."""

from masonite.providers import (AppProvider, BroadcastProvider, CacheProvider,
                                CsrfProvider, HelpersProvider, MailProvider,
                                QueueProvider, RouteProvider, SassProvider,
                                SessionProvider, StatusCodeProvider,
                                UploadProvider, ViewProvider,
                                WhitenoiseProvider)

"""Providers List
Providers are a simple way to remove or add functionality for Masonite
The providers in this list are either ran on server start or when a
request is made depending on the provider. Take some time to can
learn more more about Service Providers in our documentation
"""

PROVIDERS = [
    # Framework Providers
    AppProvider,
    SessionProvider,
    RouteProvider,
    StatusCodeProvider,
    WhitenoiseProvider,
    ViewProvider,

    # Optional Framework Providers
    # SassProvider,
    # MailProvider,
    # UploadProvider,
    # QueueProvider,
    # CacheProvider,
    # BroadcastProvider,
    # CsrfProvider,
    # HelpersProvider,

    # Third Party Providers

    # Application Providers

]
