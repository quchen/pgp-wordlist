{-# LANGUAGE OverloadedStrings #-}

-- | Core functionality for conversion between binary formats and PGP word
--   lists.
module Data.Text.PgpWordlist.Internal.Convert where



import qualified Data.Text.PgpWordlist.Internal.AltList    as Alt
import           Data.Text.PgpWordlist.Internal.Types
import           Data.Text.PgpWordlist.Internal.Word8Bimap
import           Data.Text.PgpWordlist.Internal.Words

import qualified Data.ByteString.Lazy                      as BSL
import           Data.Text                                 (Text)
import qualified Data.Text                                 as T
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
toEvenWord = lookupL evenMap

-- | Look up the word corresponding to a byte.
toOddWord :: Word8 -> OddWord
toOddWord = lookupL oddMap



-- | Simple conversion, taking into account invalid words.
fromEvenWord :: Text -> Either TranslationError Word8
fromEvenWord word = case lookupR evenMap (EvenWord word) of
    Just i  -> Right i
    Nothing -> Left (case lookupR oddMap (OddWord word) of
        Just j  -> BadParity word j
        Nothing -> BadWord word)

-- | Simple conversion, taking into account invalid words.
fromOddWord :: Text -> Either TranslationError Word8
fromOddWord word = case lookupR oddMap (OddWord word) of
    Just i  -> Right i
    Nothing -> Left (case lookupR evenMap (EvenWord word) of
        Just j  -> BadParity word j
        Nothing -> BadWord word)



-- | Mapping from and to 'EvenWord's
evenMap :: Word8Bimap EvenWord
evenMap = unsafeConstruct (map pick12 wordList)
  where
    pick12 :: (a,b,c) -> (a,b)
    pick12 (i,e,_) = (i,e)

-- | Mapping from and to 'OddWord's
oddMap :: Word8Bimap OddWord
oddMap = unsafeConstruct (map pick13 wordList)
  where
    pick13 :: (a,b,c) -> (a,c)
    pick13 (i,_,o) = (i,o)
