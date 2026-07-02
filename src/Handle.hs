module Handle where

import Data.Text (intercalate)
import Manifest qualified
import System.Directory (getCurrentDirectory)
import System.FilePath (takeBaseName)
import System.Process (callCommand)
import Types (PackageInput (name))
import Prelude hiding (intercalate)

init :: IO ()
init = do
  putTextLn "Initializing R Package Manager project"
  defaultName <- toText . takeBaseName <$> getCurrentDirectory
  putText $ "Project name (default: " <> defaultName <> "): "
  hFlush stdout
  enteredName <- getLine
  callCommand "nix flake init --template github:pchabros/r-package-manager#app"
  Manifest.init (if enteredName == "" then defaultName else enteredName)
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
