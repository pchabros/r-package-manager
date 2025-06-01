module Handle where

import Data.Text (intercalate)
import Manifest qualified
import System.Directory (getCurrentDirectory)
import System.FilePath (takeBaseName)
import System.Process (callCommand)
import Prelude hiding (intercalate)

init :: IO ()
init = do
  putTextLn "Initializing R Package Manager project"
  defaultName <- toText . takeBaseName <$> getCurrentDirectory
  putText $ "Project name (default: " <> defaultName <> "): "
  hFlush stdout
  enteredName <- getLine
  let name = if enteredName == "" then defaultName else enteredName
  callCommand "nix flake init --template github:pchabros/r-package-manager#app"
  callCommand "direnv allow"
  file <- Manifest.read
  Manifest.save file{Manifest.name}

add :: [Text] -> IO ()
add packages = do
  file <- Manifest.read
  Manifest.save
    file{Manifest.packages = ordNub $ sort $ file.packages ++ packages}
  putTextLn $ "Adding packages: " <> intercalate ", " packages
  callCommand "direnv reload"

remove :: [Text] -> IO ()
remove packages = do
  file <- Manifest.read
  Manifest.save
    file{Manifest.packages = filter (`notElem` packages) file.packages}
  putTextLn $ "Removing packages: " <> intercalate ", " packages
  callCommand "direnv reload"
