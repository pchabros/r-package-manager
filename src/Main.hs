module Main where

import Cmd qualified
import Handle qualified
import Options.Applicative (execParser)
import Prelude hiding (intercalate)

main :: IO ()
main = do
  opts <- execParser Cmd.optsParser
  case opts.cmd of
    Cmd.Init -> Handle.init
    (Cmd.Add packages) -> Handle.add packages
    (Cmd.Remove packages) -> Handle.remove packages
