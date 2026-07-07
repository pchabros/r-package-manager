module Handle where

import Data.Text (intercalate)
import Input qualified as Inp
import Manifest qualified
import Nixpkgs (rVersions)
import System.Directory (getCurrentDirectory)
import System.FilePath (takeBaseName)
import System.Process (callCommand)
import Text.Megaparsec (parseMaybe)
import Types (PackageInput (name), pVersion)
import Prelude hiding (intercalate)

init :: IO ()
init = do
  putTextLn "Initializing R Package Manager project"
  defaultName <- toText . takeBaseName <$> getCurrentDirectory
  enteredName <- Inp.text $ "Project name (default: " <> defaultName <> "): "
  let name = if enteredName == "" then defaultName else enteredName
  rVersionRaw <- Inp.select "R version" =<< rVersions
  rVersion <- case parseMaybe pVersion (toString rVersionRaw) of
    Just v -> return v
    Nothing -> error $ "Invalid R version: " <> rVersionRaw
  Manifest.init name rVersion
  callCommand "nix flake init --template github:pchabros/r-package-manager#app"
  callCommand "git init"
  callCommand "git add ."
  callCommand "direnv allow"

add :: [PackageInput] -> IO ()
add packages = do
  Manifest.add packages
  putTextLn $
    "Adding packages: " <> intercalate ", " (show . (.name) <$> packages)
  callCommand "direnv reload"

remove :: [PackageInput] -> IO ()
remove packages = do
  Manifest.remove packages
  putTextLn $
    "Removing packages: " <> intercalate ", " (show . (.name) <$> packages)
  callCommand "direnv reload"
