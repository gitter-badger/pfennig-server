module Models where

import           Control.Applicative ((<$>))
import           Control.Monad       (mzero)
import           Data.Aeson
import           Data.Text           (Text)
import           Data.Time.Format    (defaultTimeLocale, formatTime)
import           Data.Time.LocalTime (LocalTime)

newtype ExpenditureId = ExpenditureId Int deriving (Eq, Show)
newtype ExpenditureTagId = ExpenditureTagId Int deriving (Eq, Show)

-- | batch of expenses at a specific point in time
data Expenditure = Expenditure {
       _expId        :: ExpenditureId
     , _expCreatedAt :: LocalTime
     , _expUpdatedAt :: LocalTime
     , _expFields    :: ExpenditureFields
     } deriving Show

-- | a single expense description e.g. "cellphone bill"
data ExpenditureFields = ExpenditureFields {
       _expFldDescription :: Text
     } deriving (Show)

-- | a tag to group the expenditures by
data ExpenditureTag = ExpenditureTag {
       _tagId        :: ExpenditureTagId
     , _tagTitle     :: Text
     , _tagCreatedAt :: LocalTime
     } deriving (Show)

instance ToJSON ExpenditureId where
  toJSON (ExpenditureId eid) = toJSON eid

instance ToJSON Expenditure where
  toJSON label = object [ "id" .= _expId label
                        , "createdAt" .= _expCreatedAt label
                        , "updatedAt" .= _expUpdatedAt label
                        , "description" .= desc ]
    where desc = _expFldDescription $ _expFields label

instance ToJSON ExpenditureFields where
  toJSON (ExpenditureFields desc) = object [ "description" .= desc ]

instance FromJSON ExpenditureFields where
  parseJSON (Object v) = ExpenditureFields <$> v .: "description"
  parseJSON _ = mzero

instance ToJSON LocalTime where
  toJSON = toJSON . formatTime defaultTimeLocale "%Y%m%dT%H%M%S"
