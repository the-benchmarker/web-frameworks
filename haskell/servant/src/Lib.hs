{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators   #-}
module Lib
    ( startApp
    , app
    ) where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant

type API = Get '[PlainText] String
  :<|> "user" :> Capture "id" String :> Get '[PlainText] String
  :<|> "user" :> Post '[PlainText] String

startApp :: IO ()
startApp = run 3000 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = return notMuch :<|> echoId :<|> return notMuch

notMuch :: String
notMuch = ""

echoId :: String -> Handler String
echoId = pure


