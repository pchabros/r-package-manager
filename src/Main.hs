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
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  ParserInfo,
  argument,
  command,
  execParser,
  fullDesc,
  header,
  helper,
  info,
  metavar,
  progDesc,
  str,
  subparser,
 )
import Relude qualified as BS
import System.Directory (getCurrentDirectory)
import System.FilePath (takeBaseName)
import Prelude hiding (intercalate)

-- Commands

newtype Opts = Opts {cmd :: Cmd}

data Cmd = Init | Add [Text] | Remove [Text]

initCmd :: Mod CommandFields Cmd
initCmd = command "init" (info (pure Init) (progDesc "Init r-package-manager project"))

packagesParser :: Parser [Text]
packagesParser = some (argument str (metavar "PACKAGES"))

addOpts :: Parser Cmd
addOpts = Add <$> packagesParser

addCmd :: Mod CommandFields Cmd
addCmd = command "add" (info addOpts (progDesc "Add R packages"))

removeOpts :: Parser Cmd
removeOpts = Remove <$> packagesParser

removeCmd :: Mod CommandFields Cmd
removeCmd = command "remove" (info removeOpts (progDesc "Remove R packages"))

optsParser :: ParserInfo Opts
optsParser = info (parser <**> helper) (fullDesc <> header "R Package Manager")
 where
  parser = Opts <$> subparser (initCmd <> addCmd <> removeCmd)

-- Project file

data ProjectFile = ProjectFile {name :: Text, packages :: [Text]}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

encode :: (ToJSON a) => a -> LazyByteString
encode = encodePretty' defConfig{confIndent = Spaces 2}

-------------------------

main :: IO ()
main = do
  opts <- execParser optsParser
  case opts.cmd of
    Init -> do
      putTextLn "Initializing R Package Manager project"
      defaultName <- toText . takeBaseName <$> getCurrentDirectory
      putText $ "Project name (default: " <> defaultName <> "): "
      hFlush stdout
      userName <- getLine
      let name = if userName == "" then defaultName else userName
      let content =
            encode ProjectFile{name, packages = []}
      BS.writeFileLBS "r-pm.json" content
    (Add packages) -> putTextLn $ "Adding packages: " <> intercalate ", " packages
    (Remove packages) -> putTextLn $ "Removing packages: " <> intercalate ", " packages
