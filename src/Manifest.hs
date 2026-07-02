module Manifest where

import Cran (packageLatestVersion)
import Data.Aeson (ToJSON, decode)
import Data.Aeson.Encode.Pretty (
  Config (confIndent),
  Indent (Spaces),
  defConfig,
  encodePretty',
 )
import Data.ByteString.Lazy (LazyByteString)
import Data.Map.Strict qualified as Map
import Data.Text (strip)
import System.Process (proc, readCreateProcess)
import Types (
  LockDependency (..),
  LockFile (..),
  ManifestFile (..),
  Package (..),
  PackageInput (..),
  Packages,
  Version (..),
 )
import Types qualified as M (ManifestFile (name))
import Prelude hiding (writeFile)

init :: Text -> IO ()
init name = do
  file <- readManifest
  writeFile manifestPath file{M.name = name}
  lock Map.empty

add :: [PackageInput] -> IO ()
add packages = do
  filledPackages <- mapM fillLatestVersion packages
  file <- readManifest
  let newPackages = Map.union file.packages (entries filledPackages)
  writeFile manifestPath file{packages = newPackages}
  lock newPackages

remove :: [PackageInput] -> IO ()
remove packages = do
  file <- readManifest
  let newPackages = Map.filterWithKey (\p _ -> p `notElem` ((.name) <$> packages)) file.packages
  writeFile manifestPath file{packages = newPackages}
  lock newPackages

lock :: Packages -> IO ()
lock packages = do
  dependencies <- sequence $ Map.mapWithKey lock' packages
  let file = LockFile{dependencies}
  writeFile lockPath file
 where
  lock' pname version = do
    latest <- packageLatestVersion pname
    let isLatest = formatVersion latest == version
    let url = packageUrl isLatest pname version
    hash <- processStdout "nix-prefetch-url" ["--type", "sha256", url]
    return LockDependency{version, resolved = url, integrity = hash}

processStdout :: Text -> [Text] -> IO Text
processStdout cmd args = do
  out <- readCreateProcess (proc (toString cmd) (map toString args)) ""
  return $ strip $ toText out

manifestPath :: FilePath
manifestPath = "r-pm.json"

lockPath :: FilePath
lockPath = "r-pm-lock.json"

writeFile :: (ToJSON a) => FilePath -> a -> IO ()
writeFile path file = writeFileLBS path $ encode file

readManifest :: IO ManifestFile
readManifest = do
  json <- readFileLBS manifestPath
  case decode json of
    (Just file) -> pure file
    Nothing -> error "Malformed manifest file"

fillLatestVersion :: PackageInput -> IO Package
fillLatestVersion PackageInput{name, version = Nothing} = do
  version <- packageLatestVersion name
  return Package{name, version = version}
fillLatestVersion PackageInput{name, version = Just version} = return Package{name, version}

formatVersion :: Version -> Text
formatVersion (Version major minor patch) = show major <> "." <> show minor <> "." <> show patch

entries :: [Package] -> Packages
entries ps =
  Map.fromList $ (\(Package name version) -> (name, formatVersion version)) <$> ps

encode :: (ToJSON a) => a -> LazyByteString
encode = encodePretty' defConfig{confIndent = Spaces 2}

packageUrl :: Bool -> Text -> Text -> Text
packageUrl isLatest name version =
  "https://cran.r-project.org/src/contrib/"
    <> (if isLatest then "" else "Archive/" <> name <> "/")
    <> name
    <> "_"
    <> version
    <> ".tar.gz"
