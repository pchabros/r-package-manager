module Manifest where

import Cmd (Package (..), formatVersion)
import Cran (packageLatestVersion)
import Data.Aeson (ToJSON, decode)
import Data.Aeson.Encode.Pretty (
  Config (confIndent),
  Indent (Spaces),
  defConfig,
  encodePretty',
 )
import Data.Aeson.Types (FromJSON)
import Data.ByteString.Lazy (LazyByteString)
import Data.Map.Strict qualified as Map
import Data.Text (strip)
import System.Process (proc, readCreateProcess)

type Packages = Map Text Text

data ManifestFile = ManifestFile {name :: Text, packages :: Packages}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype LockFile = LockFile {dependencies :: Map Text LockDependency}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data LockDependency = LockDependency {version :: Text, resolved :: Text, integrity :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

entries :: [Package] -> Packages
entries ps =
  Map.fromList $ (\(Package name version) -> (name, formatVersion version)) <$> ps

encode :: (ToJSON a) => a -> LazyByteString
encode = encodePretty' defConfig{confIndent = Spaces 2}

processStdout :: Text -> [Text] -> IO Text
processStdout cmd args = do
  out <- readCreateProcess (proc (toString cmd) (map toString args)) ""
  return $ strip $ toText out

packageUrl :: Bool -> Text -> Text -> Text
packageUrl isLatest name version =
  "https://cran.r-project.org/src/contrib/"
    <> (if isLatest then "" else "Archive/" <> name <> "/")
    <> name
    <> "_"
    <> version
    <> ".tar.gz"

manifestPath :: FilePath
manifestPath = "r-pm.json"

lockPath :: FilePath
lockPath = "r-pm-lock.json"

save :: ManifestFile -> IO ()
save file = writeFileLBS manifestPath $ encode file

read :: IO ManifestFile
read = do
  json <- readFileLBS manifestPath
  case decode json of
    (Just file) -> pure file
    Nothing -> error "Malformed manifest file"

lock :: Packages -> IO ()
lock packages = do
  dependencies <- sequence $ Map.mapWithKey makeLockDep packages
  let file = LockFile{dependencies}
  writeFileLBS lockPath $ encode file

makeLockDep :: Text -> Text -> IO LockDependency
makeLockDep pname version = do
  latest <- packageLatestVersion pname
  let isLatest = formatVersion latest == version
  let url = packageUrl isLatest pname version
  hash <- processStdout "nix-prefetch-url" ["--type", "sha256", url]
  return LockDependency{version, resolved = url, integrity = hash}
