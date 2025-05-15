module Manifest where

import Data.Aeson (ToJSON, decode)
import Data.Aeson.Encode.Pretty (
  Config (confIndent),
  Indent (Spaces),
  defConfig,
  encodePretty',
 )
import Data.Aeson.Types (FromJSON)
import Data.ByteString.Lazy (LazyByteString)

data File = File {name :: Text, packages :: [Text]}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

encode :: (ToJSON a) => a -> LazyByteString
encode = encodePretty' defConfig{confIndent = Spaces 2}

path :: FilePath
path = "r-pm.json"

save :: File -> IO ()
save file = writeFileLBS path $ encode file

read :: IO File
read = do
  json <- readFileLBS path
  case decode json of
    (Just file) -> pure file
    Nothing -> error "Malformed manifest file"
