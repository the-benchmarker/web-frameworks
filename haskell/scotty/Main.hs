import Web.Scotty

main :: IO ()
main = scotty 3000 $ do
  get "/" $ text ""
  get "/user/:userId" $ do
    userId <- param "userId"
    text userId
  post "/user" $ text ""
