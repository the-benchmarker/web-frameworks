"""Storage Settings."""

from masonite import env

"""Storage Driver
The default driver you will like to use for storing uploads. You may add
additional drivers as you need or pip install additional drivers.

Supported: 'disk', 's3', 'rackspace', 'googlecloud', 'azure'
"""

DRIVER = env("STORAGE_DRIVER", "disk")

"""Storage Drivers
Different drivers you can use for storing file uploads.
"""

DRIVERS = {
    "disk": {"location": "storage/uploads"},
    "s3": {
        "client": env("S3_CLIENT", "AxJz..."),
        "secret": env("S3_SECRET", "HkZj..."),
        "bucket": env("S3_BUCKET", "s3bucket"),
    },
    "rackspace": {
        "username": env("RACKSPACE_USERNAME", "username"),
        "secret": env("RACKSPACE_SECRET", "3cd5b0e8..."),
        "container": env("RACKSPACE_CONTAINER", "masonite"),
        "region": env("RACKSPACE_REGION", "IAD"),
        "location": "http://03c8...rackcdn.com/",
    },
    "azure": {
        "name": env("AZURE_NAME", "masonite"),
        "secret": env("AZURE_SECRET", "RykG8qsa4kTOddF=="),
        "connection": env(
            "AZURE_CONNECTION", "DefaultEndpointsProtocol=https;AccountName=..."
        ),
        "container": env("AZURE_CONTAINER", "masonite"),
        "location": "https://masonite.blob.core.windows.net/container/",
    },
}


"""Static Files
Put anywhere you keep your static assets in a key, value dictionary here
The key will be the folder you put your assets in relative to the root
and the value will be the alias you wish to have in your templates.
You may have multiple aliases of the same name

Example will be the static assets folder at /storage/static
and an alias of <img src="/static/image.png">
"""

STATICFILES = {
    # folder          # template alias
    "storage/static": "static/",
    "storage/compiled": "static/",
    "storage/uploads": "static/",
    "storage/public": "/",
}
