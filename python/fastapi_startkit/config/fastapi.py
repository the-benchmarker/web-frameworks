import dataclasses

from fastapi_startkit.environment import env


@dataclasses.dataclass
class FastAPIConfig:
    app_url: str = dataclasses.field(default_factory=lambda: env("APP_URL", "http://0.0.0.0:3000"))
    host: str = dataclasses.field(default_factory=lambda: env("APP_HOST", "0.0.0.0"))
    port: int = dataclasses.field(default_factory=lambda: env("APP_PORT", 3000))
    reload: bool = dataclasses.field(default_factory=lambda: env("APP_RELOAD", False))
    reload_dirs: list | None = None
    reload_excludes: list = dataclasses.field(default_factory=list)
