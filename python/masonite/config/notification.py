from masonite.environment import env


DRIVERS = {
    "slack": {
        "token": env("SLACK_TOKEN", ""),  # used for API mode
        "webhook": env("SLACK_WEBHOOK", ""),  # used for webhook mode
    },
    "vonage": {
        "key": env("VONAGE_KEY", ""),
        "secret": env("VONAGE_SECRET", ""),
        "sms_from": env("VONAGE_SMS_FROM", "+33000000000"),
    },
    "database": {
        "connection": "sqlite",
        "table": "notifications",
    },
}

DRY = False
