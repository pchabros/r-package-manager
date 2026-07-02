module Parsers where

import Text.Megaparsec (Parsec)
import Text.Megaparsec.Char (alphaNumChar, char, numberChar)
import Text.Read (read)
import Types (PackageInput (..), Version (..))

type MParser = Parsec Void String

pNumber :: MParser Int
pNumber = read <$> many numberChar

pVersion :: MParser Version
pVersion =
  Version
    <$> pNumber
    <*> (char '.' *> pNumber)
    <*> (char '.' *> pNumber)

pPackage :: MParser PackageInput
pPackage =
  PackageInput
    <$> (toText <$> many (alphaNumChar <|> char '.'))
    <*> optional (char '@' *> pVersion)
