module Data.Text.PgpWordlist.Internal.Types where



import           Data.Text                              (Text)
import           Data.Text.PgpWordlist.Internal.AltList
import           Data.Word



-- | Abstract representation of a PGP word list.
newtype PgpWordlist = PgpWordlist (AltList EvenWord OddWord)
    deriving (Eq, Ord, Show)



-- | Possible translation errors from a list of PGP words to binary format.
data TranslationError =
      BadWord Text         -- ^ Word is not recognized
    | BadParity Text Word8 -- ^ Word is recognized, but from the wrong
                           --   alphabet. Duplicates, omissions, and neighbour
                           --   transpositions are often cause for this.
    deriving (Eq, Ord, Show)

-- | Word that is supposed to occur in an even position
newtype EvenWord = EvenWord { unEvenWord :: Text }
    deriving (Eq, Ord, Show)


-- | Word that is supposed to occur in an odd position
newtype OddWord = OddWord { unOddWord :: Text }
    deriving (Eq, Ord, Show)
