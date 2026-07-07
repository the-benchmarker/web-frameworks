{-
module Main

Main entry point for the production-grade Servant web application.

This module simply imports and starts the application defined in Lib.
-}, {-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Lib (startApp)

-- | Main application entry point
-- Delegates to Lib.startApp which handles all the production-grade setup
main :: IO ()
main = startApp
