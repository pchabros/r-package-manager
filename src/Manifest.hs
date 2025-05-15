module Manifest where

import Data.Aeson (ToJSON)
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

save :: File -> IO ()
save file = writeFileLBS "r-pm.json" $ encode file
