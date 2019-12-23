
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- Usage: curl -vvv -u servant:server localhost:8081/users

module Simple where

import Servant
import Data.Text
import Text.Read
import Data.Time (UTCTime)
import GHC.Generics
import Data.Aeson
import Network.Wai.Handler.Warp
import Data.Time.Clock
import Servant.API.BasicAuth

import Network.Wai.Middleware.Servant.Options

type UserAPI = BasicAuth "foo-realm" User :> "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

data SortBy = Age | Name deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON SortBy

genericToUrlPieceViaShow :: Show a =>  a -> Text
genericToUrlPieceViaShow = Data.Text.pack . show

genericFromUrlPieceViaRead :: Read a =>  Text -> Either Text a
genericFromUrlPieceViaRead = readE "lol" . Data.Text.unpack

readE :: Read b => a -> String -> Either a b
readE x y = maybe (Left x) Right $ readMaybe y

instance ToHttpApiData   SortBy where toUrlPiece    = genericToUrlPieceViaShow
instance FromHttpApiData SortBy where parseUrlPiece = genericFromUrlPieceViaRead

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: UTCTime
  }
  deriving (Generic)

instance ToJSON User

authCheck :: BasicAuthCheck User
authCheck = BasicAuthCheck check
  where
    check (BasicAuthData username password) = do
      t <- getCurrentTime
      if username == "servant" && password == "server"
        then return (Authorized (User "servant user" 99 "foo@bar.com" t))
        else return Unauthorized

basicAuthServerContext :: Context (BasicAuthCheck User ': '[])
basicAuthServerContext = authCheck :. EmptyContext

dummyUsers :: IO [User]
dummyUsers = do
  t1 <- getCurrentTime
  return [ User "Chris" 21 "chris@email.com" t1 ]

app :: [User] -> Application
app us = serveWithContext
  (Proxy :: Proxy UserAPI)
  basicAuthServerContext $ return $
  \case Nothing   -> return us
        Just Age  -> return us
        Just Name -> return (Prelude.reverse us)

main :: IO ()
main = do
  us <- dummyUsers
  -- Works: run 8081 (app us)
  run 8081 (provideOptions (Proxy :: Proxy UserAPI) (app us))
