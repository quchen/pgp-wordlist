{-# LANGUAGE OverloadedStrings #-}

-- | Core functionality for conversion between binary formats and PGP word
--   lists.
module Data.Text.PgpWordlist.Internal.Convert where



import qualified Data.Text.PgpWordlist.Internal.AltList as Alt
import           Data.Text.PgpWordlist.Internal.Types
import           Data.Text.PgpWordlist.Internal.Words

import           Data.Bimap                             (Bimap, (!))
import qualified Data.Bimap                             as BM

import qualified Data.ByteString.Lazy                   as BSL
import           Data.Text                              (Text)
import qualified Data.Text                              as T
import           Data.Word



-- | Inverse of 'fromText', modulo whitespace count.
toText :: BSL.ByteString -> Text
toText = T.intercalate " "
       . Alt.toList
       . Alt.bimap (unEvenWord . toEvenWord) (unOddWord . toOddWord)
       . Alt.fromList
       . BSL.unpack



-- | Convert a text of whitespace-separated words to their binary
--   representation. The whitespace splitting behaviour is given by 'T.words'.
fromText :: Text -> Either TranslationError BSL.ByteString
fromText = fmap (BSL.pack . Alt.toList)
         . Alt.bitraverse fromEvenWord fromOddWord
         . Alt.fromList
         . T.words



-- | Look up the word corresponding to a byte.
toEvenWord :: Word8 -> EvenWord
toEvenWord = (evenMap !) -- evenMap is total, so the lookup is safe

-- | Look up the word corresponding to a byte.
toOddWord :: Word8 -> OddWord
toOddWord = (oddMap !) -- oddMap is total, so the lookup is safe



-- | Simple conversion, taking into account invalid words.
fromEvenWord :: Text -> Either TranslationError Word8
fromEvenWord word = case BM.lookupR (EvenWord word) evenMap of
    Just i  -> Right i
    Nothing -> Left (case BM.lookupR (OddWord word) oddMap of
        Just j  -> BadParity word j
        Nothing -> BadWord word)

-- | Simple conversion, taking into account invalid words.
fromOddWord :: Text -> Either TranslationError Word8
fromOddWord word = case BM.lookupR (OddWord word) oddMap of
    Just i  -> Right i
    Nothing -> Left (case BM.lookupR (EvenWord word) evenMap of
        Just j  -> BadParity word j
        Nothing -> BadWord word)



-- | Mapping from and to 'EvenWord's
evenMap :: Bimap Word8 EvenWord
evenMap = BM.fromList (map pick12 wordList)
  where
    pick12 :: (a,b,c) -> (a,b)
    pick12 (i,e,_) = (i,e)

-- | Mapping from and to 'OddWord's
oddMap :: Bimap Word8 OddWord
oddMap = BM.fromList (map pick13 wordList)
  where
    pick13 :: (a,b,c) -> (a,c)
    pick13 (i,_,o) = (i,o)
