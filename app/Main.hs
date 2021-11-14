{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

import Control.Lens
import Data.Aeson (decode, encode)
import Data.Aeson.Lens
import Data.Aeson.TH (Options (..), defaultOptions, deriveJSON)
import qualified Data.Aeson.Types as At
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as BSL
import Data.Char (toLower)
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust, isNothing)
import qualified Data.Text as T
import Data.Text.ICU.Replace (replaceAll)
import GHC.Generics
import qualified Network.HTTP.Simple as HS
import qualified Network.HTTP.Types as HT
import Text.Regex.Posix

data Submit = Submit
  { user_id :: String,
    language :: String,
    count :: Int
  }
  deriving (Show, Generic)

deriveJSON defaultOptions {fieldLabelModifier = drop 0} ''Submit

main = do
  a <- HS.httpBS "https://kenkoooo.com/atcoder/resources/lang.json"
  let url = map (BSL.pack . T.unpack) $ T.words $ T.replace "},{" "} {" $ T.init $ T.tail $ T.pack $ BS.unpack $ HS.getResponseBody a
      json = head url
      decoded1 = decode (json) :: Maybe Submit
      dee = map decode url :: [Maybe Submit]
  print $ decoded1