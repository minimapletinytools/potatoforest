module Lib (

) where

import           Data.Text as T

data Item = Item {
  itemId      :: T.Text
  , itemTitle :: T.Text
  , itemDesc  :: T.Text
}
