module Main where

import Cmd qualified
import Data.Text (intercalate)
import Handle qualified
import Options.Applicative (execParser)
import Prelude hiding (intercalate)

main :: IO ()
main = do
  opts <- execParser Cmd.optsParser
  case opts.cmd of
    Cmd.Init -> Handle.init
    (Cmd.Add packages) -> putTextLn $ "Adding packages: " <> intercalate ", " packages
    (Cmd.Remove packages) -> putTextLn $ "Removing packages: " <> intercalate ", " packages
