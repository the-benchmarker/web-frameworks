# This is a simplified server setup for the Supranim framework
# https://github.com/supranim/supranim

import std/[httpcore, macros, macrocache, options]

import pkg/kapsis/[framework, runtime]
import pkg/supranim
import pkg/supranim/controller
import pkg/supranim/core/[request, router, response]

include ./routes

import ./controller/[pages, errors]

initApplication()
initHttpRouter()

App.configs = newOrderedTable[string, YAMLObject]()
App.configs["server"] = parseYAML("port: 3000")

App.run()