module Types where

import Data.Text
import Database.Persist.Sql

type Url = String

-- | Event id, unique inside a feed
newtype Eid = Eid Text
    deriving (Eq, Ord, PersistField, PersistFieldSql, Show)
