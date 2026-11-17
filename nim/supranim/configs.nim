import std/[json, tables]
import pkg/supranim/core/application

proc configureServer*(app: Application) =
  # Supranim 0.1.12 shares mutable regex captures between request workers.
  # Use one worker until upstream makes dynamic route matching thread-safe.
  app.configs = newOrderedTable[string, Configuration]()
  app.configs["app"] = Configuration(format: cfgJson, jsonDoc: %*{
    "server": {
      "address": "0.0.0.0",
      "port": 3000,
      "threads": 1
    }
  })
