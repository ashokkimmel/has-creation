module Control.Has.Config
  ( Config(..)
  , defaultConfig
  ) where

-- | Configuration for the code generators in this package.
-- The 'nameOverride' field, if present, overrides the automatically
-- derived type name (from 'nameBase') used when generating class and
-- method names.
data Config = Config
  { nameOverride :: Maybe String
  }

-- | The default configuration used by the convenience generators.
defaultConfig :: Config
defaultConfig = Config { nameOverride = Nothing }
