{-
module Lib

A production-grade Servant web application demonstrating best practices for
Haskell web development using the Servant framework.

Features:
- Type-level API specification
- Environment-based configuration
- Security headers middleware
- Request logging (disabled in production)
- Error handling with custom error types
- Health check endpoint
- CORS support with restrictive defaults
- Request validation
- Request size limits to prevent DoS
- Gzip compression for performance
-}, {-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Lib
  ( startApp
  , app
  , appWithConfig
  )
where

import Control.Exception (handle, SomeException)
import Control.Monad (unless, when)
import Data.Aeson (ToJSON, Value(..), object, (.=))
import Data.ByteString (ByteString)
import Data.Char (isAlphaNum)
import Data.Default (def)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.Wai (Application, Middleware)
import Network.Wai.Handler.Warp (Settings, defaultSettings, runSettings)
import Network.Wai.Middleware.AddHeaders (addHeaders)
import Network.Wai.Middleware.Cors (CorsResourcePolicy(..), cors, simpleHeaders)
import Network.Wai.Middleware.Gzip (gzip, gzipFiles)
import Network.Wai.Middleware.RequestSizeLimit (requestSizeLimit)
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import System.Environment (lookupEnv)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
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
  
  when (env == "production") $ do
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
-- Restrictive CORS policy for security. Customize corsOrigins for your domain.
corsMiddleware :: Middleware
corsMiddleware = cors $ def
  { corsOrigins = Just (["https://yourdomain.com"], True)
  , corsMethods = ["GET", "POST", "PUT", "DELETE", "OPTIONS"]
  , corsRequestHeaders = simpleHeaders
  , corsExposedHeaders = ["Location"]
  , corsMaxAge = Just 86400  -- 24 hours
  }

-- | Request size limit middleware to prevent DoS attacks
requestSizeLimitMiddleware :: Int -> Middleware
requestSizeLimitMiddleware size = requestSizeLimit size

-- | Simple request logging middleware
-- In production, logging is disabled for performance
-- In development, logs to stderr
loggingMiddleware :: AppConfig -> Middleware
loggingMiddleware config app req respond = do
  let method = Wai.requestMethod req
      path = T.unpack (Wai.rawPathInfo req)
      query = T.unpack (Wai.rawQueryString req)
      fullPath = path <> if null query then "" else "?" <> query
  
  -- Only log in non-production environments
  unless (configEnv config == "production") $ do
    hPutStrLn stderr $ "[" <> method <> "] " <> fullPath
  
  app req respond

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
    
  -- Logging: Simple request logging (disabled in production)
  , loggingMiddleware config
  ]

-- | Type-level API specification
-- This defines our application's routes using Servant's combinators
data API routes = API
  { _health :: routes :- "health" :> Get '[JSON] HealthResponse
  , _get    :: routes :-             Get  '[PlainText] Text
  , _echo   :: routes :- "user" :> Capture "id" Text :> Get  '[PlainText] Text
  , _post   :: routes :- "user" :>         Post '[PlainText] Text
  }
  deriving (Generic)

-- | Health check response type
data HealthResponse = HealthResponse
  { hrStatus :: Text
  , hrVersion :: Text
  , hrEnvironment :: Text
  } deriving (Generic, Show)

instance ToJSON HealthResponse

-- | API type synonym for better type signatures
type API = ToServantApi API

-- | Server implementation for the API
server :: AppConfig -> API AsServer
server config = API
  { _health = healthHandler config
  , _get    = notMuch
  , _echo   = echoId
  , _post   = notMuch
  }

-- | Health check handler
healthHandler :: AppConfig -> Handler HealthResponse
healthHandler config = pure $ HealthResponse
  { hrStatus = "healthy"
  , hrVersion = "1.0.0"
  , hrEnvironment = configEnv config
  }

-- | Handler for endpoints that return empty responses
notMuch :: Handler Text
notMuch = pure ""

-- | Handler for the echo endpoint with parameter validation
echoId :: Text -> Handler Text
echoId userId = do
  -- Validate userId is not empty and is alphanumeric
  unless (not (T.null userId) && all (\(c) -> isAlphaNum c || c == '-' || c == '_') userId) $ do
    throwError $ err400 { errBody = "Invalid user ID format" }
  pure userId

-- | Create WAI Application with all middleware applied
appWithConfig :: AppConfig -> Application
appWithConfig config =
  let middlewares = appMiddleware config
      servantApp = genericServe (Proxy :: Proxy API) (server config)
  in foldr (.) id middlewares servantApp

-- | Create WAI Application with default configuration
app :: Application
app = appWithConfig defaultAppConfig

-- | Warp server settings optimized for production
warpSettings :: AppConfig -> Warp.Settings
warpSettings config = defaultSettings
  { Warp.setPort = configPort config
  , Warp.setHost = configHost config
  , Warp.setTimeout = configTimeout config
  , Warp.setFdCacheDuration = 60  -- Cache file descriptors for 60 seconds
  , Warp.setFileInfoCacheDuration = 600  -- Cache file info for 10 minutes
  }

-- | Start the application with configuration from environment
startApp :: IO ()
startApp = do
  -- Load configuration
  config <- loadConfig
  
  hPutStrLn stderr $ "Starting server on port " <> show (configPort config)
  hPutStrLn stderr $ "Environment: " <> T.unpack (configEnv config)
  hPutStrLn stderr $ "Host: " <> BS8.unpack (configHost config)
  
  -- Create application with middleware
  let application = appWithConfig config
      settings = warpSettings config
  
  -- Run server with exception handling
  handle (\e -> do
    hPutStrLn stderr $ "Fatal error: " <> show (e :: SomeException)
    exitFailure) $ do
      Warp.runSettings settings application
