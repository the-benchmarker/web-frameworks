from masonite.environment import env


DRIVERS = {
    "default": env("MAIL_DRIVER", "terminal"),
    "smtp": {
        "host": env("MAIL_HOST"),
        "port": env("MAIL_PORT"),
        "username": env("MAIL_USERNAME"),
        "password": env("MAIL_PASSWORD"),
    },
    "mailgun": {
        "domain": env("MAILGUN_DOMAIN"),
        "secret": env("MAILGUN_SECRET"),
    },
}
