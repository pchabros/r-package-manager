module Input where

import Data.List (lookup)

text :: Text -> IO Text
text label = do
  putText label
  hFlush stdout
  getLine

select :: Text -> [Text] -> IO Text
select label options = do
  putTextLn label
  traverse_ printOption enumOpt
  putText "Choose: "
  hFlush stdout
  ind <- toString <$> getLine
  maybe
    (putTextLn "Enter valid number." >> select label options)
    return
    (readMaybe ind >>= (`lookup` enumOpt))
 where
  enumOpt = zip [1 :: Int ..] options
  printOption (i, o) = putTextLn $ show i <> ") " <> o
