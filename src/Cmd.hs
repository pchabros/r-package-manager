module Cmd where

import Options.Applicative
  ( CommandFields,
    Mod,
    Parser,
    ParserInfo,
    argument,
    command,
    fullDesc,
    header,
    helper,
    info,
    maybeReader,
    metavar,
    progDesc,
    subparser,
  )
import Text.Megaparsec (Parsec, parseMaybe)
import Text.Megaparsec.Char (alphaNumChar, char, numberChar)
import Text.Read (read)

newtype Opts = Opts {cmd :: Cmd}

data Cmd = Init | Add [Package] | Remove [Package]

data Package = Package {name :: Text, version :: Version} deriving stock (Show)

data Version = Version
  { major :: Int,
    minor :: Int,
    patch :: Int
  }
  deriving stock (Show)

initCmd :: Mod CommandFields Cmd
initCmd = command "init" (info (pure Init) (progDesc "Init r-package-manager project"))

packagesParser :: Parser [Package]
packagesParser = some (argument pkg (metavar "PACKAGES"))
  where
    pkg = maybeReader $ parseMaybe pPackage

type MParser = Parsec Void String

pNumber :: MParser Int
pNumber = read <$> many numberChar

pVersion :: MParser Version
pVersion =
  Version
    <$> pNumber
    <*> (char '.' *> pNumber)
    <*> (char '.' *> pNumber)

pPackage :: MParser Package
pPackage =
  Package
    <$> (toText <$> many (alphaNumChar <|> char '.'))
    <*> (char '@' *> pVersion)

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
