"""Queue Settings."""

from masonite import env

"""Queue Driver
Queues are an excellent way to send intensive and time consuming tasks
into the background to improve performance of your application.

Supported: 'async', 'amqp'
"""

DRIVER = env('QUEUE_DRIVER', 'async')

"""Queue Drivers
Put any configuration settings for your drivers in this configuration setting.
"""

DRIVERS = {
    'amqp': {
        'username': env('QUEUE_USERNAME', 'guest'),
        'vhost': env('QUEUE_VHOST', ''),
        'password': env('QUEUE_PASSWORD', 'guest'),
        'host': env('QUEUE_HOST', 'localhost'),
        'port': env('QUEUE_PORT', '5672'),
        'channel': env('QUEUE_CHANNEL', 'default'),
    }
}
