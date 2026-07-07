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
import Text.Megaparsec (parseMaybe)
import Types (
  Package (..),
  PackageName (..),
  UrlName (UrlName),
  Version,
  formatVersion,
  pVersion,
 )

newtype PackageData = PackageData {version :: Text}
  deriving stock (Generic, Show)

capitalize :: String -> String
capitalize "" = ""
capitalize (c : cs) = toUpper c : cs

instance FromJSON PackageData where
  parseJSON = genericParseJSON defaultOptions{fieldLabelModifier = capitalize}

packageLatestVersion :: PackageName -> IO Version
packageLatestVersion PackageName{name} = do
  request <- parseRequest $ "https://crandb.r-pkg.org/" <> toString name
  response <- httpLBS request
  let body = decode $ getResponseBody response :: Maybe PackageData
  case body of
    Just pkg -> case parseMaybe pVersion (toString pkg.version) of
      Just v -> return v
      Nothing -> error $ "Can't parse version for package \"" <> name <> "\""
    Nothing -> error $ "Can't get info for package \"" <> name <> "\""

packageUrl :: Bool -> Package -> UrlName
packageUrl isLatest Package{name = PackageName{name}, version} =
  UrlName $
    "https://cran.r-project.org/src/contrib/"
      <> (if isLatest then "" else "Archive/" <> name <> "/")
      <> name
      <> "_"
      <> formatVersion version
      <> ".tar.gz"
