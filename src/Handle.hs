module Handle where

import Cmd (Package (..), PackageInput (..))
import Cran (packageLatestVersion)
import Data.Map.Strict qualified as Map
import Data.Text (intercalate)
import Manifest qualified
import System.Directory (getCurrentDirectory)
import System.FilePath (takeBaseName)
import System.Process (callCommand)
import Prelude hiding (intercalate)

fillLatestVersion :: PackageInput -> IO Package
fillLatestVersion PackageInput{name, version = Nothing} = do
  version <- packageLatestVersion name
  return Package{name, version = version}
fillLatestVersion PackageInput{name, version = Just v} = return Package{name, version = v}

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

add :: [PackageInput] -> IO ()
add packages = do
  filledPackages <- mapM fillLatestVersion packages
  file <- Manifest.read
  let newPackages = Map.union file.packages (Manifest.entries filledPackages)
  Manifest.save
    file{Manifest.packages = newPackages}
  Manifest.lock newPackages
  putTextLn $
    "Adding packages: " <> intercalate ", " (show . (.name) <$> packages)
  callCommand "direnv reload"

remove :: [PackageInput] -> IO ()
remove packages = do
  file <- Manifest.read
  let newPackages =
        Map.filterWithKey
          (\p _ -> p `notElem` ((.name) <$> packages))
          file.packages
  Manifest.save file{Manifest.packages = newPackages}
  Manifest.lock newPackages
  putTextLn $
    "Removing packages: " <> intercalate ", " (show . (.name) <$> packages)
  callCommand "direnv reload"
