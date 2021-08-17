import Web.Scotty

main :: IO ()
main = scotty 8080 $ do
  get "/" $ text ""
  get "/user/:userId" $ do
    userId <- param "userId"
    text userId
  post "/user" $ text ""
