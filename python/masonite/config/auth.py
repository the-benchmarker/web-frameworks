from app.models.User import User

GUARDS = {
    "default": "web",
    "web": {"model": User},
    "password_reset_table": "password_resets",
    "password_reset_expiration": 1440,  # in minutes. 24 hours. None if disabled
}
