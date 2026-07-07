@echo off
setlocal enabledelayedexpansion

set LANGUAGES=c clojure cpp crystal csharp d dart elixir fsharp gleam go gogo guile haskell java javascript julia kotlin lua luau nim objc ocaml perl php python r ruby rust scala swift v zig

set TOTAL=0

for %%L in (%LANGUAGES%) do (
    if exist "%%L" (
        set COUNT=0
        for /d %%F in ("%%L\*"^) do (
            set /a COUNT+=1
        )
        set TOTAL=!TOTAL!+!COUNT!
        echo %%L: !COUNT! frameworks
    )
)

echo.
echo Total: %TOTAL%
