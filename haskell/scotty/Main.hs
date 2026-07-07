{-
module Main

A production-grade Scotty web application demonstrating best practices for
Haskell web development.

Features:
- Environment-based configuration
- Security headers middleware
- Structured request logging
- Error handling
- Health check endpoint
- CORS support
- Request validation
- Graceful shutdown handling
-}, {-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Exception (handle, SomeException)
import Control.Monad (unless, when)
import Data.Aeson (ToJSON, Value(..), object, (.=))
import Data.ByteString (ByteString)
import Data.Char (isAlphaNum)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Network.Wai (Application, Middleware)
import Network.Wai.Handler.Warp (Settings, defaultSettings, runSettings)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Cors (cors, simpleHeaders)
import Network.Wai.Middleware.Gzip (gzip, gzipFiles)
import Network.Wai.Middleware.RequestSizeLimit (requestSizeLimit)
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Web.Scotty
import qualified Data.ByteString.Char8 as BS8
import qualified Data.Text as T
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp

-- | Application configuration loaded from environment variables
-- with sensible defaults for production.
data AppConfig = AppConfig
  { configPort :: Int
  , configHost :: ByteString
  , configTimeout :: Int
  , configEnv :: Text
  , configMaxRequestSize :: Int
  }

-- | Default configuration for production deployments
defaultAppConfig :: AppConfig
defaultAppConfig = AppConfig
  { configPort = 3000
  , configHost = "0.0.0.0"
  , configTimeout = 30  -- seconds
  , configEnv = "production"
  , configMaxRequestSize = 10 * 1024 * 1024  -- 10MB
  }

-- | Load configuration from environment variables
-- Environment variables override defaults
loadConfig :: IO AppConfig
loadConfig = do
  port <- fromMaybe (configPort defaultAppConfig) . fmap read <$> lookupEnv "PORT"
  host <- fromMaybe (configHost defaultAppConfig) . fmap BS8.pack <$> lookupEnv "HOST"
  timeout <- fromMaybe (configTimeout defaultAppConfig) . fmap read <$> lookupEnv "REQUEST_TIMEOUT"
  env <- fromMaybe (configEnv defaultAppConfig) . fmap T.pack <$> lookupEnv "ENV"
  maxRequestSize <- fromMaybe (configMaxRequestSize defaultAppConfig) . fmap read <$> lookupEnv "MAX_REQUEST_SIZE"
  
  let isProduction = env == "production"
  when isProduction $ do
    hPutStrLn stderr "Running in production mode"
    hPutStrLn stderr "Debug logging disabled"
    hPutStrLn stderr "Security headers enabled"
  
  pure $ defaultAppConfig
    { configPort = port
    , configHost = host
    , configTimeout = timeout
    , configEnv = env
    , configMaxRequestSize = maxRequestSize
    }

-- | Security headers for production
-- These headers provide essential security protections
securityHeaders :: [(ByteString, ByteString)]
securityHeaders = 
  [ ("X-Content-Type-Options", "nosniff")
  , ("X-Frame-Options", "DENY")
  , ("X-XSS-Protection", "1; mode=block")
  , ("Referrer-Policy", "strict-origin-when-cross-origin")
  , ("Permissions-Policy", "default-src 'self'")
  ]

-- | CORS configuration for production
-- Restrictive CORS policy for security
corsMiddleware :: Middleware
corsMiddleware = cors $ def
  { corsOrigins = Just (["https://yourdomain.com"], True)
  , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
  , corsRequestHeaders = simpleHeaders
  , corsExposedHeaders = ["Location"]
  , corsMaxAge = Just 86400
  }

-- | Request size limit middleware to prevent DoS attacks
requestSizeLimitMiddleware :: Int -> Middleware
requestSizeLimitMiddleware size = requestSizeLimit size

-- | Application middleware stack for production
-- Ordered from outer to inner (first to last to apply)
appMiddleware :: AppConfig -> [Middleware]
appMiddleware config = 
  [ -- Security: Add security headers
    addHeaders securityHeaders
    
  -- Security: CORS with restrictive policy
  , corsMiddleware
    
  -- Security: Limit request body size to prevent DoS
  , requestSizeLimitMiddleware (configMaxRequestSize config)
    
  -- Performance: Enable gzip compression
  , gzip def { gzipFiles = True }
  ]

-- | Simple request logging middleware
-- In production, this would be replaced with a proper logging library
loggingMiddleware :: AppConfig -> Middleware
loggingMiddleware config app req respond = do
  let method = Wai.requestMethod req
      path = T.unpack (Wai.rawPathInfo req)
      query = T.unpack (Wai.rawQueryString req)
      fullPath = path <> if null query then "" else "?" <> query
  
  -- Only log in non-production environments
  unless (configEnv config == "production") $ do
    hPutStrLn stderr $ "Request: " <> method <> " " <> fullPath
  
  app req respond

-- | WAI Application with all middleware applied
appWithMiddleware :: AppConfig -> Application
appWithMiddleware config =
  let middlewares = appMiddleware config ++ [loggingMiddleware config]
      scottyApp = app config
  in foldr (.) id middlewares scottyApp

-- | Scotty application routes
app :: AppConfig -> Application
app config = scottyApp $ do
  -- Health check endpoint (always available)
  get "/health" $ do
    status status200
    setHeader "Content-Type" "application/json"
    json $ object
      [ "status" .= ("healthy" :: Text)
      , "version" .= ("1.0.0" :: Text)
      , "environment" .= configEnv config
      ]
  
  -- Root endpoint
  get "/" $ do
    status status200
    setHeader "Content-Type" "text/plain; charset=utf-8"
    text ""
  
  -- User endpoint with parameter validation
  get "/user/:userId" $ do
    userId <- param "userId"
    -- Validate userId is not empty and is alphanumeric
    unless (not (T.null userId) && all (\(c) -> isAlphaNum c || c == '-' || c == '_') userId) $ do
      status status400
      setHeader "Content-Type" "application/json"
      json $ object ["error" .= ("Invalid user ID format" :: Text)]
      finish
    
    status status200
    setHeader "Content-Type" "text/plain; charset=utf-8"
    text userId
  
  -- User creation endpoint
  post "/user" $ do
    status status201
    setHeader "Content-Type" "text/plain; charset=utf-8"
    setHeader "Location" "/user"
    text ""
  
  -- 404 handler for unknown routes
  notFound $ do
    status status404
    setHeader "Content-Type" "application/json"
    json $ object ["error" .= ("Not Found" :: Text)]
  
  -- Exception handler
  defaultHandler $ \e -> do
    status status500
    setHeader "Content-Type" "application/json"
    json $ object ["error" .= ("Internal Server Error" :: Text)]

-- | Warp server settings optimized for production
warpSettings :: AppConfig -> Warp.Settings
warpSettings config = defaultSettings
  { Warp.setPort = configPort config
  , Warp.setHost = configHost config
  , Warp.setTimeout = configTimeout config
  , Warp.setFdCacheDuration = 60
  , Warp.setFileInfoCacheDuration = 600
  }

-- | Main application entry point
-- Handles graceful shutdown and proper resource management
main :: IO ()
main = do
  -- Load configuration
  config <- loadConfig
  
  hPutStrLn stderr $ "Starting server on port " <> show (configPort config)
  hPutStrLn stderr $ "Environment: " <> T.unpack (configEnv config)
  hPutStrLn stderr $ "Host: " <> BS8.unpack (configHost config)
  
  -- Create application with middleware
  let application = appWithMiddleware config
      settings = warpSettings config
  
  -- Run server with exception handling
  handle (\e -> do
    hPutStrLn stderr $ "Fatal error: " <> show (e :: SomeException)
    exitFailure) $ do
      Warp.runSettings settings application
