{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- A middleware to respond to Options requests for a servant app
-- very helpful when trying to deal with pre-flight CORS requests.
--
module Network.Wai.Middleware.Servant.Options (provideOptions) where

import Servant
import Servant.Foreign
import Network.Wai
import Data.Text hiding (null, zipWith, length)
import Network.HTTP.Types.Method
import Data.Maybe
import Data.List (nub)
import Network.HTTP.Types
import qualified Data.ByteString as B

provideOptions :: (GenerateList NoContent (Foreign NoContent api), HasForeign NoTypes NoContent api)
               => Proxy api -> Middleware
provideOptions apiproxy app req cb
  | rmeth == "OPTIONS" = optional cb prior pinfo mlist
  | otherwise          = prior
  where
  rmeth = requestMethod req :: Method
  pinfo = pathInfo      req :: [ Text ]
  mlist = listFromAPI (Proxy :: Proxy NoTypes) (Proxy :: Proxy NoContent) apiproxy
  prior = app req cb

optional :: (Response -> r) -> r -> [Text] -> [Req NoContent] -> r
optional cb prior ts rs
  | null methods = prior
  | otherwise    = cb (buildResponse methods)
  where
  methods = mapMaybe (getMethod ts) rs

getMethod :: [Text] -> Req NoContent -> Maybe Method
getMethod rs ps
  | sameLength && matchingSegments = Just (_reqMethod ps)
  | otherwise                      = Nothing
  where
  pattern          = _path $ _reqUrl ps
  sameLength       = length rs == length pattern
  matchingSegments = and $ zipWith matchSegment rs pattern

matchSegment :: Text -> Segment NoContent -> Bool
matchSegment a (Segment (Static (PathSegment b)) ) | a /= b = False
matchSegment _ _                                            = True

buildResponse :: [Method] -> Response
buildResponse ms = responseBuilder s h mempty
  where
  s = Status 200 "OK"
  m = B.intercalate ", " ("OPTIONS" : nub ms)
  h = [ ("Allow", m) ]

