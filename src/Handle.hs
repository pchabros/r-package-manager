module Handle where

import Cmd (Package)
import Manifest qualified
import System.Directory (getCurrentDirectory)
import System.FilePath (takeBaseName)
import System.Process (callCommand, createProcess, proc)
import Prelude hiding (intercalate)

-- -- nix hash convert --hash-algo sha256 $(nix-prefetch-url 'https://cran.r-project.org/src/contrib/Archive/dplyr/dplyr_1.1.3.tar.gz')
-- processStdout :: Text -> [Text] -> IO Text
-- processStdout cmd args = do
--   (_, Just hout, _, _) <- createProcess (proc (toString cmd) (map toString args))
--   return $ show hout
--
-- packageUrl :: Text -> Text -> Text -- TODO: create `data Package`
-- packageUrl package version =
--   "https://cran.r-project.org/src/contrib/Archive/"
--     <> package
--     <> "/"
--     <> package
--     <> "_"
--     <> version
--     <> ".tar.gz"
--
-- prefetchPackage :: Text -> Text -> IO Text
-- prefetchPackage package version = processStdout "nix-prefetch-url" [packageUrl package version]
--
-- --

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
  Manifest.save file {Manifest.name}

add :: [Package] -> IO ()
add packages = do
  putStrLn "########################"
  print packages
  putStrLn "########################"

-- file <- Manifest.read
-- Manifest.save
--   file {Manifest.packages = ordNub $ sort $ file.packages ++ packages}
-- putTextLn $ "Adding packages: " <> intercalate ", " packages
-- callCommand "direnv reload"

remove :: [Package] -> IO ()
remove packages = do
  file <- Manifest.read
  Manifest.save
    file -- {Manifest.packages = filter (`notElem` packages) file.packages}
    -- putTextLn $ "Removing packages: " <> intercalate ", " packages
  callCommand "direnv reload"
