{-# LANGUAGE CPP #-}

-- | A type of list that contains two alternating element types.
module Data.Text.PgpWordlist.Internal.AltList (
      AltList(..)

    -- * Construction, deconstruction
    , fromList
    , toList

    -- * Bifunctor
    , first
    , second
    , bimap

    -- * Bitraversable
    , bisequence
    , bitraverse
) where



import           Control.Applicative

#if MIN_VERSION_base(4,8,0)
import qualified Data.Bifunctor      as Bi
#endif



-- | List of elements of alternating element types.
data AltList a b = Nil | a :<> AltList b a
    deriving (Eq, Ord, Show)

infixr 5 :<> -- same as (:)



-- | Simple conversion function. Inverse of 'toList'.
fromList :: [a] -> AltList a a
fromList = foldr (:<>) Nil

-- | Simple conversion function. Inverse of 'fromList'.
toList :: AltList a a -> [a]
toList Nil = []
toList (x :<> xs) = x : toList xs



-- | Map over every other element of an 'AltList', starting with the first
--   entry.
--
-- @
-- 'first' f ≡ 'bimap' f 'id'
-- @
first :: (a -> a') -> AltList a b -> AltList a' b
first _ Nil = Nil
first f (x :<> Nil) = f x :<> Nil
first f (x :<> y :<> ys) = f x :<> y :<> first f ys



-- | Map over every other element of an 'AltList', starting with the second
--   entry.
--
-- @
-- 'second' g ≡ 'bimap' 'id' g
-- @
second :: (b -> b') -> AltList a b -> AltList a b'
second _ Nil = Nil
second g (x :<> xs) = x :<> first g xs



-- | Map over the both types of entries of an 'AltList'.
--
-- @
-- 'bimap' f g ≡ 'second' g . 'first' f
-- @
bimap :: (a -> a') -> (b -> b') -> AltList a b -> AltList a' b'
bimap _ _ Nil = Nil
bimap f g (x :<> xs) = f x :<> bimap g f xs



#if MIN_VERSION_base(4,8,0)
instance Bi.Bifunctor AltList where
    first = first
    second = second
    bimap = bimap
#endif



-- | The 'Data.Bifunctor.Bifunctor' analogon to 'Data.Traversable.sequenceA'.
bisequence :: Applicative f => AltList (f a) (f b) -> f (AltList a b)
bisequence Nil = pure Nil
bisequence (x :<> xs) = liftA2 (:<>) x (bisequence xs)

-- | The 'Data.Bifunctor.Bifunctor' analogon to 'Data.Traversable.traverse'.
bitraverse :: Applicative f => (a -> f c) -> (b -> f d) -> AltList a b -> f (AltList c d)
bitraverse _ _ Nil = pure Nil
bitraverse f g (x :<> xs) = liftA2 (:<>) (f x) (bitraverse g f xs)
