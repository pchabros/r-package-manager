module Cran where

import Data.Aeson (
  FromJSON (parseJSON),
  Options (fieldLabelModifier),
  decode,
  defaultOptions,
  genericParseJSON,
 )
import Data.Char (toUpper)
import Network.HTTP.Client.Conduit (parseRequest)
import Network.HTTP.Simple (getResponseBody, httpLBS)
import Parsers (pVersion)
import Text.Megaparsec (parseMaybe)
import Types (Version)

newtype PackageData = PackageData {version :: Text}
  deriving stock (Generic, Show)

capitalize :: String -> String
capitalize "" = ""
capitalize (c : cs) = toUpper c : cs

instance FromJSON PackageData where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = capitalize}

packageLatestVersion :: Text -> IO Version
packageLatestVersion pname = do
  request <- parseRequest $ "https://crandb.r-pkg.org/" <> toString pname
  response <- httpLBS request
  let body = decode $ getResponseBody response :: Maybe PackageData
  case body of
    Just pkg -> case parseMaybe pVersion (toString pkg.version) of
      Just v -> return v
      Nothing -> error $ "Can't parse version for package \"" <> pname <> "\""
    Nothing -> error $ "Can't get info for package \"" <> pname <> "\""
