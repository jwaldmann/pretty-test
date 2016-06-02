-- | enumerate (and memoize) elements of types.
-- like the original SmallCheck, but using Size rather than Depth.
-- size (Foo x y) = 1 + size x + size y,
-- depth (Foo x y) = 1 + max (depth x) (depth y)

-- The following is needed for generic instances
{-# LANGUAGE DefaultSignatures, TypeOperators, FlexibleContexts,
  TypeSynonymInstances, FlexibleInstances, OverlappingInstances #-}

module Series where

import Data.Monoid
import GHC.Generics

import Control.Applicative

-- | this is an infinite list of lists.
-- each individual lists is finite, and may even be empty.
newtype Series a = Series [[a]]

-- | lazy (infinite) list of all values.
-- if the contents is actually finite (since only a finite number
-- of element lists are non-empty) then this will block (blackhole)
-- instead of reaching []
contents :: Series a -> [a]
contents (Series xss) = concat xss

-- | Not exactly a monoid. But modulo permutations, it is.
instance Monoid (Series a) where
  mempty = Series $ repeat []
  mappend (Series xss) (Series yss) =
    let merge [] ys = ys
        merge (x:xs) ys = x : merge ys xs
    in Series $ zipWith merge xss yss

-- | size of pair is sum of size of elements.
(><) :: Series a -> Series b -> Series (a,b)
Series xss >< Series yss = Series $ do
  d <- [ 0 .. ]
  -- FIXME: improve efficiency here:
  zipWith cartesian (take d xss) (reverse $ take d yss)

cartesian :: [a] -> [b] -> [(a,b)]
cartesian xs ys = do x <- xs ; y <- ys ; return (x,y)

instance Functor Series where
  fmap f (Series xss) = Series $ map (fmap f) xss

instance Applicative Series where
  pure x = Series $ [x] : repeat []

-- | increase size of each element (use this to model constructor application)
shift :: Series a -> Series a
shift (Series xss) = Series $ [] : xss

cons0 x = shift $ pure x
cons1 f = shift $ f <$> series
cons2 f = shift $ uncurry f <$> series

instance Serial Int where
  series = Series $ [0] : map (\ n -> [n, negate n]) [1..]

instance Serial a => Serial [a]
instance Serial ()
instance ( Serial a , Serial b ) => Serial (a,b)
instance Serial Bool

-- * generics code
-- is copied from
-- http://hackage.haskell.org/package/smallcheck-1.1.1/docs/src/Test-SmallCheck-Series.html

class Serial a where
  series :: Series a
  default series :: (Generic a, GSerial (Rep a)) => Series a
  series = to <$> gSeries

class GSerial f where
  gSeries :: Series (f a)
  
instance GSerial f => GSerial (M1 i c f) where
  gSeries = M1 <$> gSeries
  {-# INLINE gSeries #-}

instance Serial c => GSerial (K1 i c) where
  gSeries = K1 <$> series
  {-# INLINE gSeries #-}

instance GSerial U1 where
  gSeries = pure U1
  {-# INLINE gSeries #-}

instance (GSerial a, GSerial b) => GSerial (a :*: b) where
  gSeries = uncurry (:*:) <$> gSeries >< gSeries
  {-# INLINE gSeries #-}

instance (GSerial a, GSerial b) => GSerial (a :+: b) where
  gSeries = (L1 <$> gSeries) <> (R1 <$> gSeries)
  {-# INLINE gSeries #-}

instance GSerial f => GSerial (C1 c f) where
  gSeries = M1 <$> shift gSeries
  {-# INLINE gSeries #-}


