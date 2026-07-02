module Types where

import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)

data PackageInput = PackageInput {name :: Text, version :: Maybe Version}
data Package = Package {name :: Text, version :: Version}

data Version = Version
  { major :: Int
  , minor :: Int
  , patch :: Int
  }
  deriving stock (Eq)

type Packages = Map Text Text

data ManifestFile = ManifestFile {name :: Text, packages :: Packages}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype LockFile = LockFile {dependencies :: Map Text LockDependency}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data LockDependency = LockDependency {version :: Text, resolved :: Text, integrity :: Text}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
