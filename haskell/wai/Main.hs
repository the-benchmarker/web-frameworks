{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Wai
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types.Status
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Data.ByteString.Lazy (ByteString, fromChunks)

textToLBS :: Text -> ByteString
textToLBS = fromChunks . pure . encodeUtf8

app :: Application
app rq cb = case (requestMethod rq, pathInfo rq) of 
    (_, []) -> cb $ responseLBS status200 [] ""
    ("GET", ["user", t]) -> cb $ responseLBS status200 [] (textToLBS t)
    _ -> cb $ responseLBS status404 [] ""

main :: IO ()
main = run 3000 app
