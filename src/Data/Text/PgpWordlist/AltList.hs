-- | Defines a type of list that containins two alternating element types.
module Data.Text.PgpWordlist.AltList (
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

-- | List of elements of alternating element types.
data AltList a b = Nil | Cons a (AltList b a)
    deriving (Eq, Ord, Show)



-- | Simple conversion function. Inverse of 'toList'.
fromList :: [a] -> AltList a a
fromList = foldr Cons Nil

-- | Simple conversion function. Inverse of 'fromList'.
toList :: AltList a a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs



-- | Map over the first component of an 'AltList'.
--
-- @
-- 'first' f ≡ 'bimap' f 'id'
-- @
first :: (a -> a') -> AltList a b -> AltList a' b
first _ Nil = Nil
first f (Cons x Nil) = Cons (f x) Nil
first f (Cons x (Cons y ys)) = Cons (f x) (Cons y (first f ys))



-- | Map over the second component of an 'AltList'.
--
-- @
-- 'second' g ≡ 'bimap' 'id' g
-- @
second :: (b -> b') -> AltList a b -> AltList a b'
second _ Nil = Nil
second _ (Cons x Nil) = Cons x Nil
second g (Cons x (Cons y ys)) = Cons x (Cons (g y) (second g ys))



-- | Map over the both components of an 'AltList'.
--
-- @
-- 'bimap' f g ≡ 'second' g . 'first' f
-- @
bimap :: (a -> a') -> (b -> b') -> AltList a b -> AltList a' b'
bimap _ _ Nil = Nil
bimap f g (Cons x xs) = Cons (f x) (bimap g f xs)



-- | Monomorphic bisequence.
bisequence :: Applicative f => AltList (f a) (f b) -> f (AltList a b)
bisequence Nil = pure Nil
bisequence (Cons x xs) = Cons <$> x <*> bisequence xs

-- | Monomorphic bitraverse.
bitraverse :: Applicative f => (a -> f c) -> (b -> f d) -> AltList a b -> f (AltList c d)
bitraverse _ _ Nil = pure Nil
bitraverse f g (Cons x xs) = Cons <$> f x <*> bitraverse g f xs