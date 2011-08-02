module Purecoin.Crypto.ZModule where

import Data.Monoid (Monoid, mempty, mappend)

(<+>) :: (Monoid m) => m -> m -> m
(<+>) = mappend

class Monoid m => ZModule m where
  double :: m -> m
  double x = x <+> x

  opposite :: m -> m
  opposite x = mempty <-> x

  (<->) :: m -> m -> m
  x <-> y = x <+> (opposite y)

(*+) :: ZModule m => m -> Integer -> m
_ *+ 0 = mempty
x *+ 1 = x
x *+ n | n < 0  = opposite (x *+ (negate n))
       | even n = double (x *+ (n `div` 2))
       | odd n  = x <+> (x *+ (pred n))