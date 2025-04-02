{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib
  ( startApp,
    app,
  )
where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import Servant.API.Generic
import Servant.Server.Generic
import Data.Text (Text)

data API routes = API
  { _get  :: routes :-                                Get  '[PlainText] Text
  , _echo :: routes :- "user" :> Capture "id" Text :> Get  '[PlainText] Text
  , _post :: routes :- "user" :>                      Post '[PlainText] Text
  }
  deriving (Generic)

startApp :: IO ()
startApp = run 3000 app

app :: Application
app = genericServe server

api :: Proxy (ToServantApi API)
api = genericApi (Proxy :: Proxy API)

server :: API AsServer
server = API
  { _get  = notMuch
  , _echo = echoId
  , _post = notMuch
  }

notMuch :: Handler Text
notMuch = pure ""

echoId :: Text -> Handler Text
echoId = pure
