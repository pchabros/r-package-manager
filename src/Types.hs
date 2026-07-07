module Types where

import Data.Aeson (
  FromJSON (parseJSON),
  FromJSONKey,
  ToJSON (toJSON),
  ToJSONKey,
  Value (String),
  withText,
 )
import Text.Megaparsec (Parsec, parseMaybe)
import Text.Megaparsec.Char (alphaNumChar, char, numberChar)
import Text.Read (read)

newtype BranchName = BranchName {name :: Text}
  deriving newtype (Show, IsString, ToJSON, FromJSON)
newtype CommitName = CommitName {name :: Text}
  deriving newtype (Show, IsString, ToJSON, FromJSON)
newtype PackageName = PackageName {name :: Text}
  deriving newtype
    (Show, IsString, Eq, Ord, FromJSON, FromJSONKey, ToJSON, ToJSONKey)
newtype UrlName = UrlName {name :: Text}
  deriving newtype (Show, IsString, ToJSON, FromJSON)
newtype HashName = HashName {name :: Text}
  deriving newtype (Show, IsString, ToJSON, FromJSON)

data PackageInput = PackageInput {name :: PackageName, version :: Maybe Version}

data Package = Package {name :: PackageName, version :: Version}
  deriving stock (Eq, Ord, Show)

data Version = Version
  { major :: Int
  , minor :: Int
  , patch :: Int
  }
  deriving stock (Eq, Ord, Show)

formatVersion :: Version -> Text
formatVersion (Version major minor patch) =
  show major <> "." <> show minor <> "." <> show patch

formatMajorMinor :: Version -> Text
formatMajorMinor (Version major minor _) =
  show major <> "." <> show minor

instance FromJSON Version where -- TODO: Try to move to separate file
  parseJSON = withText "Version" $ \t ->
    case parseMaybe pVersion $ toString t of
      Just version -> return version
      Nothing -> fail $ "Invalid version: " <> toString t

instance ToJSON Version where
  toJSON = String . formatVersion

data ManifestFile = ManifestFile {name :: Text, rVersion :: Version, packages :: [Package]}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data LockFile = LockFile
  {nixpkgs :: NixpkgsDependency, dependencies :: Map PackageName LockDependency}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data NixpkgsDependency = NixpkgsDependency
  { branch :: BranchName
  , resolved :: UrlName
  , integrity :: HashName
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data LockDependency = LockDependency {resolved :: UrlName, integrity :: HashName}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance FromJSON Package where
  parseJSON = withText "Package" $ \t -> do
    case parseMaybe pPackage $ toString t of
      Just (PackageInput name (Just version)) -> return $ Package name version
      _ -> fail $ "Invalid package: " <> toString t

instance ToJSON Package where
  toJSON Package{name = PackageName{name}, version} = String $ name <> "@" <> formatVersion version

type MParser = Parsec Void String -- TODO: Try to move to separate file

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
    <$> (PackageName . toText <$> many (alphaNumChar <|> char '.'))
    <*> optional (char '@' *> pVersion)
