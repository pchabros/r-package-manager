module Main where

import Data.Aeson (FromJSON, ToJSON)
import Data.Aeson.Encode.Pretty (
  Config (confIndent),
  Indent (Spaces),
  defConfig,
  encodePretty',
 )
import Data.ByteString.Lazy (LazyByteString)
import Data.Text (intercalate)
import Options.Applicative (execParser)
import Relude qualified as BS
import System.Directory (getCurrentDirectory)
import System.FilePath (takeBaseName)
import Prelude hiding (intercalate)

import Cmd qualified

data ProjectFile = ProjectFile {name :: Text, packages :: [Text]}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

encode :: (ToJSON a) => a -> LazyByteString
encode = encodePretty' defConfig{confIndent = Spaces 2}

main :: IO ()
main = do
  opts <- execParser Cmd.optsParser
  case opts.cmd of
    Cmd.Init -> do
      putTextLn "Initializing R Package Manager project"
      defaultName <- toText . takeBaseName <$> getCurrentDirectory
      putText $ "Project name (default: " <> defaultName <> "): "
      hFlush stdout
      userName <- getLine
      let name = if userName == "" then defaultName else userName
      let content =
            encode ProjectFile{name, packages = []}
      BS.writeFileLBS "r-pm.json" content
    (Cmd.Add packages) -> putTextLn $ "Adding packages: " <> intercalate ", " packages
    (Cmd.Remove packages) -> putTextLn $ "Removing packages: " <> intercalate ", " packages
