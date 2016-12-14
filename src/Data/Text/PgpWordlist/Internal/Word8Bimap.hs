-- | Bimap between 'Word8' an an arbitrary (ordered) type. Values of type
-- 'Word8Bimap' have to be constructed carefully to contain values for
-- all 256 'Word8' values, or the API will not be safe to use.
module Data.Text.PgpWordlist.Internal.Word8Bimap (
      Word8Bimap
    , unsafeConstruct
    , lookupL
    , lookupR
) where



import           Data.Map    (Map)
import qualified Data.Map    as M
import           Data.Vector (Vector, (!))
import qualified Data.Vector as V

import Data.Tuple
import Data.Word



-- | Bimap where one of the keys is (of the size of) a 'Word8'.
data Word8Bimap a = Bimap (Vector a) (Map a Word8)



-- | Create a 'Word8Bimap' from an association list.
--
-- The list must contain all 'Word8' in order to make the API operating on
-- the result safe.
unsafeConstruct :: Ord a => [(Word8, a)] -> Word8Bimap a
unsafeConstruct assocs = Bimap vec inverseMap
  where
    vec = V.fromList (map snd assocs)
    inverseMap = M.fromList (map swap assocs)



-- | Get the value corresponding to an index.
lookupL :: Word8Bimap a -> Word8 -> a
lookupL (Bimap l _) i = l ! fromIntegral i

-- | Get the index corresponding to a value, if present.
lookupR :: Ord a => Word8Bimap a -> a -> Maybe Word8
lookupR (Bimap _ r) i = M.lookup i r
