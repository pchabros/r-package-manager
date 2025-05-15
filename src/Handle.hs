module Handle where

import Data.Text (intercalate)
import Manifest qualified
import System.Directory (getCurrentDirectory)
import System.FilePath (takeBaseName)
import Prelude hiding (intercalate)

init :: IO ()
init = do
  putTextLn "Initializing R Package Manager project"
  defaultName <- toText . takeBaseName <$> getCurrentDirectory
  putText $ "Project name (default: " <> defaultName <> "): "
  hFlush stdout
  userName <- getLine
  let name = if userName == "" then defaultName else userName
  Manifest.save Manifest.File{Manifest.name, Manifest.packages = []}

add :: [Text] -> IO ()
add packages = do
  file <- Manifest.read
  Manifest.save
    file{Manifest.packages = ordNub $ sort $ file.packages ++ packages}
  putTextLn $ "Adding packages: " <> intercalate ", " packages

remove :: [Text] -> IO ()
remove packages = do
  file <- Manifest.read
  Manifest.save
    file{Manifest.packages = filter (`notElem` packages) file.packages}
  putTextLn $ "Removing packages: " <> intercalate ", " packages
