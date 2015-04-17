module Data.Text.PgpWordlist (
    -- * Main functionality
      fromText
    , fromLazyBS

    -- * Might be useful
    , toEvenWord
    , fromEvenWord
    , toOddWord
    , fromOddWord
) where

import Data.Text.PgpWordlist.Tables
