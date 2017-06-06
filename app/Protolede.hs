module Protolede
  ( module X
  , String
  -- * utilities
  , length
  , tshow
  , stringify
  ) where

import           Protolude as X hiding (length)
import qualified Protolude

import           Data.Text (Text)
import qualified Data.Text as T

import           GHC.Base  (String)

length :: (Num a, Foldable t) => t b -> a
length = fromIntegral . Protolude.length

tshow :: Show a => a -> Text
tshow = T.pack . show

stringify :: (Text -> Text) -> String -> String
stringify f = T.unpack . f . T.pack
