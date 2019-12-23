
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- from https://docs.servant.dev/en/stable/tutorial/ApiType.html

module Simple where

import Servant
import Data.Text
import Text.Read
import Data.Time (UTCTime)
import GHC.Generics
import Data.Aeson
import Network.Wai.Handler.Warp
import Data.Time.Clock

import Network.Wai.Middleware.Servant.Options

type UserAPI =  "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

data SortBy = Age | Name deriving (Show, Read, Eq, Ord, Generic)

instance ToJSON SortBy

genericToUrlPieceViaShow :: Show a =>  a -> Text
genericToUrlPieceViaShow = Data.Text.pack . show

genericFromUrlPieceViaRead :: Read a =>  Text -> Either Text a
genericFromUrlPieceViaRead = readE "lol" . Data.Text.unpack

readE :: Read b => a -> String -> Either a b
readE x y = maybe (Left x) Right $ readMaybe y

instance ToHttpApiData   SortBy where toUrlPiece   = genericToUrlPieceViaShow
instance FromHttpApiData SortBy where
  parseUrlPiece = genericFromUrlPieceViaRead

data User = User
  { name :: String
  , age :: Int
  , email :: String
  , registration_date :: UTCTime
  }
  deriving (Generic)

instance ToJSON User

dummyUsers :: IO [User]
dummyUsers = do
  t1 <- getCurrentTime
  return [ User "Chris" 21 "chris@email.com" t1 ]

app :: [User] -> Application
app us = serve
  (Proxy :: Proxy UserAPI) $
  \case Nothing   -> return us
        Just Age  -> return us
        Just Name -> return (Prelude.reverse us)

main :: IO ()
main = do
  us <- dummyUsers
  run 8081 (provideOptions (Proxy :: Proxy UserAPI) (app us))
