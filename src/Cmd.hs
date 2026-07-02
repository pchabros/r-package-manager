module Cmd where

import Options.Applicative (
  CommandFields,
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
import Parsers (pPackage)
import Text.Megaparsec (parseMaybe)
import Types (PackageInput (..))

newtype Opts = Opts {cmd :: Cmd}

data Cmd = Init | Add [PackageInput] | Remove [PackageInput]

initCmd :: Mod CommandFields Cmd
initCmd = command "init" (info (pure Init) (progDesc "Init r-package-manager project"))

packagesParser :: Parser [PackageInput]
packagesParser = some (argument pkg (metavar "PACKAGES"))
 where
  pkg = maybeReader $ parseMaybe pPackage

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
