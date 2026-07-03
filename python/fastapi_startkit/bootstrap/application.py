import os
from pathlib import Path

from fastapi_startkit import Application

from config.fastapi import FastAPIConfig
from providers.fastapi_provider import FastAPIProvider

# The framework resolves its runtime environment from APP_ENV (or a .env file);
# the benchmark ships neither, so pin a default before the app boots.
os.environ.setdefault("APP_ENV", "production")

app: Application = Application(
    base_path=str(Path(__file__).resolve().parent.parent),
    providers=[
        (FastAPIProvider, FastAPIConfig),
    ],
)
