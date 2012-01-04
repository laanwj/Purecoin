module Data.NEList
       ( NEList(..), listToNEList, headTailView
       , (<|), (|>), neAppend, appendNE
       , F.toList
       ) where

import Data.Monoid (mappend)
import Control.Applicative ((<$>))
import qualified Data.Foldable as F

data NEList a = NENil a
              | NECons a (NEList a)
              deriving Eq

instance Functor NEList where
  fmap f (NENil a) = NENil (f a)
  fmap f (NECons a l) = NECons (f a) (fmap f l)

instance F.Foldable NEList where
  fold (NENil a) = a
  fold (NECons a l) = a `mappend` (F.fold l)
  foldMap f (NENil a) = f a
  foldMap f (NECons a l) = f a `mappend` (F.foldMap f l)

instance (Show a) => Show (NEList a) where
  show l = show (F.toList l)

listToNEList :: [a] -> Maybe (NEList a)
listToNEList [] = Nothing
listToNEList [a] = Just $ NENil a
listToNEList (a:l) = NECons a <$> listToNEList l

headTailView :: NEList a -> (a, [a])
headTailView (NENil a)     = (a, [])
headTailView (NECons a ht) = (a, h:t)
 where
   (h,t) = headTailView ht

(|>) :: [a] -> a -> NEList a
[]    |> a = NENil a
(h:t) |> a = NECons h (t |> a)

(<|) :: a -> [a] -> NEList a
a <| l = go (listToNEList l)
 where
   go Nothing    = NENil a
   go (Just nel) = NECons a nel

neAppend :: NEList a -> [a] -> NEList a
(NENil a)    `neAppend` l = a <| l
(NECons a b) `neAppend` l = NECons a (b `neAppend` l)

appendNE :: [a] -> NEList a -> NEList a
[]    `appendNE` l = l
(h:t) `appendNE` l = NECons h (t `appendNE` l)