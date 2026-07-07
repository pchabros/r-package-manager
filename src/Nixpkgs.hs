module Nixpkgs where

import Data.Map.Strict qualified as Map
import Data.Text (splitOn, strip, stripPrefix)

import Data.Map.Strict (keys)
import System.Process (proc, readCreateProcess)
import Text.Megaparsec (parseMaybe)
import Text.Regex.TDFA ((=~))
import Types (
  BranchName (..),
  CommitName (..),
  HashName (HashName),
  PackageName (..),
  UrlName (..),
  Version,
  formatVersion,
  pVersion,
 )

branches :: IO [BranchName]
branches =
  map BranchName
    . sort
    . filter (\b -> b > "nixos-21" && b =~ ("^nixos-[0-9]{2}\\.[0-9]{2}$" :: Text))
    . mapMaybe (viaNonEmpty last . splitOn "/")
    . lines
    <$> processStdout
      "git"
      [ "ls-remote"
      , "--heads"
      , "https://github.com/NixOS/nixpkgs.git"
      , "refs/heads/nixos-*"
      ]

packageVersion :: PackageName -> BranchName -> IO Version
packageVersion package branch =
  fromMaybe
    (error $ "Can't find " <> package.name <> " version on branch " <> branch.name)
    . parseMaybe pVersion
    . toString
    <$> processStdout
      "nix"
      [ "eval"
      , "--raw"
      , "github:nixos/nixpkgs/" <> branch.name <> "#" <> package.name <> ".version"
      ]

rPackageVersion :: PackageName -> BranchName -> IO Version
rPackageVersion package branch =
  fromMaybe
    (error $ "Can't find " <> package.name <> " version on branch " <> branch.name)
    . parseMaybe pVersion
    . toString
    . fromMaybe (error $ "R package name has prefix different than" <> prefix)
    . stripPrefix prefix
    <$> processStdout
      "nix"
      [ "eval"
      , "--raw"
      , "github:nixos/nixpkgs/"
          <> branch.name
          <> "#rPackages."
          <> package.name
          <> ".name"
      ]
 where
  prefix = "r-" <> package.name <> "-"

branchForRVersion :: IO (Map Version BranchName)
branchForRVersion = do
  br <- branches
  rv <- traverse (packageVersion ("R" :: PackageName)) br
  return $ Map.fromList $ zip rv br

rVersions :: IO [Text] -- TODO: Decide between `formatVersion` and `formatMajorMinor`
rVersions = reverse . map formatVersion . keys <$> branchForRVersion

_urlHash :: Bool -> UrlName -> IO HashName
_urlHash unpack url =
  HashName
    <$> processStdout
      "nix-prefetch-url"
      (["--unpack" | unpack] ++ ["--type", "sha256", url.name])

fileHash :: UrlName -> IO HashName
fileHash = _urlHash False

tarHash :: UrlName -> IO HashName
tarHash = _urlHash True

branchCommit :: BranchName -> IO CommitName
branchCommit branch = do
  out <-
    processStdout
      "git"
      [ "ls-remote"
      , "https://github.com/NixOS/nixpkgs.git"
      , "refs/heads/" <> branch.name
      ]
  case words out of
    (commit : _) -> return $ CommitName commit
    _ -> error $ "Could not resolve branch " <> branch.name <> " commit"

nixpkgsUrl :: CommitName -> UrlName
nixpkgsUrl commit =
  UrlName $
    "https://github.com/nixos/nixpkgs/archive/" <> commit.name <> ".tar.gz"

processStdout :: Text -> [Text] -> IO Text
processStdout cmd args = do
  out <- readCreateProcess (proc (toString cmd) (map toString args)) ""
  return $ strip $ toText out
