from fastapi_startkit.fastapi import FastAPIProvider as BaseFastAPIProvider

from routes.api import public


class FastAPIProvider(BaseFastAPIProvider):
    def boot(self) -> None:
        super().boot()
        self.app.include_router(public)
