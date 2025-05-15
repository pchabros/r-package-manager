module Handle where

import Manifest qualified
import System.Directory (getCurrentDirectory)
import System.FilePath (takeBaseName)

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
  Manifest.save file{Manifest.packages = sort $ file.packages ++ packages}
