module Manifest where

import Cran (packageLatestVersion, packageUrl)
import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Aeson.Encode.Pretty (
  Config (confIndent),
  Indent (Spaces),
  defConfig,
  encodePretty',
 )
import Data.ByteString.Lazy (LazyByteString)
import Data.Map.Strict qualified as Map
import Nixpkgs (
  branchCommit,
  branchForRVersion,
  fileHash,
  nixpkgsUrl,
  rPackageVersion,
  tarHash,
 )
import Types (
  BranchName,
  LockDependency (..),
  LockFile (..),
  ManifestFile (..),
  NixpkgsDependency (..),
  Package (..),
  PackageInput (..),
  Version,
  formatVersion,
 )
import Types qualified as M (ManifestFile (name))
import Prelude hiding (readFile, writeFile)

init :: Text -> Version -> IO ()
init name rVersion = do
  writeFile manifestPath ManifestFile{M.name, rVersion, packages = []}
  branch <- readBranch -- TODO: Imperative - try to refactor
  initLock branch

add :: [PackageInput] -> IO ()
add packages = do
  filledPackages <- mapM fillLatestVersion packages
  file <- readManifest
  let newPackages = ordNub $ file.packages ++ filledPackages
  writeFile manifestPath file{packages = newPackages}
  lockPackages newPackages

remove :: [PackageInput] -> IO ()
remove packages = do
  file <- readManifest
  let newPackages = filter (\p -> p.name `notElem` ((.name) <$> packages)) file.packages
  writeFile manifestPath file{packages = newPackages}
  lockPackages newPackages

initLock :: BranchName -> IO ()
initLock branch = do
  commit <- branchCommit branch
  let resolved = nixpkgsUrl commit
  integrity <- tarHash resolved
  let nixpkgs = NixpkgsDependency{branch, resolved, integrity}
  writeFile lockPath LockFile{nixpkgs, dependencies = Map.empty}

lockPackages :: [Package] -> IO ()
lockPackages packages = do
  dependencies <- Map.fromList <$> mapM lock' packages
  file <- readLock
  writeFile lockPath file{dependencies}
 where
  lock' p = do
    latest <- packageLatestVersion p.name
    let isLatest = latest == p.version
    let url = packageUrl isLatest p
    hash <- fileHash url
    return
      (p.name, LockDependency{resolved = url, integrity = hash})

manifestPath :: FilePath
manifestPath = "r-pm.json"

lockPath :: FilePath
lockPath = "r-pm-lock.json"

writeFile :: (ToJSON a) => FilePath -> a -> IO ()
writeFile path file = writeFileLBS path $ encode file

readFile :: (FromJSON a) => FilePath -> IO (Maybe a)
readFile path = decode <$> readFileLBS path

readManifest :: IO ManifestFile
readManifest =
  readFile manifestPath
    >>= maybe
      (error "Malformed manifest file")
      return

readLock :: IO LockFile
readLock =
  readFile lockPath
    >>= maybe
      (error "Malformed lock file")
      return

readBranch :: IO BranchName
readBranch = do
  file <- readManifest
  branch <- Map.lookup file.rVersion <$> branchForRVersion
  case branch of
    Just b -> return b
    Nothing -> error $ "Can't find branch for R version: " <> formatVersion file.rVersion

fillLatestVersion :: PackageInput -> IO Package
fillLatestVersion PackageInput{name, version = Nothing} = do
  version <- rPackageVersion name =<< readBranch
  return Package{name, version = version}
fillLatestVersion PackageInput{name, version = Just version} = return Package{name, version}

encode :: (ToJSON a) => a -> LazyByteString
encode = encodePretty' defConfig{confIndent = Spaces 2}
