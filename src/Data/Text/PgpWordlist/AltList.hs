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

-- Alternating list type.
data AltList a b = Nil | Cons a (AltList b a)
    deriving (Eq, Ord, Show)

-- first f = bimap f id
first :: (a -> a') -> AltList a b -> AltList a' b
first _ Nil = Nil
first f (Cons x Nil) = Cons (f x) Nil
first f (Cons x (Cons y ys)) = Cons (f x) (Cons y (first f ys))

-- second g = bimap id g
second :: (b -> b') -> AltList a b -> AltList a b'
second _ Nil = Nil
second _ (Cons x Nil) = Cons x Nil
second g (Cons x (Cons y ys)) = Cons x (Cons (g y) (second g ys))

-- bimap f g = second g . first f
bimap :: (a -> a') -> (b -> b') -> AltList a b -> AltList a' b'
bimap _ _ Nil = Nil
bimap f g (Cons x xs) = Cons (f x) (bimap g f xs)

fromList :: [a] -> AltList a a
fromList = foldr Cons Nil

toList :: AltList a a -> [a]
toList Nil = []
toList (Cons x xs) = x : toList xs

bisequence :: Applicative f => AltList (f a) (f b) -> f (AltList a b)
bisequence Nil = pure Nil
bisequence (Cons x xs) = Cons <$> x <*> bisequence xs

bitraverse :: Applicative f => (a -> f c) -> (b -> f d) -> AltList a b -> f (AltList c d)
bitraverse _ _ Nil = pure Nil
bitraverse f g (Cons x xs) = Cons <$> f x <*> bitraverse g f xs