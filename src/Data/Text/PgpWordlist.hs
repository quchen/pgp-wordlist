-- | Translate between binary data and a human-readable collection of words.
module Data.Text.PgpWordlist (
      toText
    , fromText
    , TranslationError(..)
) where

import Data.Text.PgpWordlist.Internal.Tables
