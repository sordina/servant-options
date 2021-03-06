
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- Usage: curl -vvv -u servant:server localhost:8081/users

{-

Example currently fails with:

    • No instance for (Servant.Foreign.Internal.GenerateList
                         NoContent (Servant.Foreign.Internal.Foreign NoContent UserAPI))
        arising from a use of ‘provideOptions’

Failing Instance of Class: http://hackage.haskell.org/package/servant-foreign-0.15/docs/Servant-Foreign-Internal.html#t:GenerateList

-}

module BasicAuth2 where

import Servant
import Data.Text
import Text.Read
import Data.Time (UTCTime)
import GHC.Generics
import Data.Aeson
import Network.Wai.Handler.Warp
import Data.Time.Clock

import Network.Wai.Middleware.Servant.Options

type UserAPI' = "users" :> QueryParam "sortby" SortBy :> Get '[JSON] [User]

type UserAPI = BasicAuth "foo-realm" User :> UserAPI'

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
  run 8081 (provideOptions (Proxy :: Proxy UserAPI') (app us))
