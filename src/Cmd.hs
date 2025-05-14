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
  metavar,
  progDesc,
  str,
  subparser,
 )

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
