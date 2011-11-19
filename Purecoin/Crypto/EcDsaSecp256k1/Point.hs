module Purecoin.Crypto.EcDsaSecp256k1.Point
       ( Point, mkPoint, isInfinity, getx, gety
       , {-a,-} b
       , Fp, p, sqrtFp
       , Fn, scale, shamirsTrick
       , castFpToFn, uncastFnToFp
       ) where

import Data.Word (Word8)
import Data.Bits (shiftR, (.|.), (.&.))
import Data.List (unfoldr)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Monoid (Monoid, mempty, mappend)
import Data.Ratio (numerator, denominator)
import Control.Monad (guard,unless)
import Data.ByteString (pack, unpack)
import Data.Serialize ( Serialize, Put
                      , get, getWord8, getBytes
                      , put, putWord8, putByteString
                      )
import Purecoin.Utils (nonNegativeByteStringBE, integerToNByteStringBE)
import Purecoin.Crypto.ZModule (ZModule, double, (<+>), opposite, (*+))

-- size of the finite field the elliptic curve is over
p :: Integer
p = 2^256 - 2^32 - 2^9 - 2^8 - 2^7 - 2^6 - 2^4 - 1

-- parameters of the elliptic curve
{-
a :: Fp
a = 0
-}

b :: Fp
b = 7

-- the order of the curve
n :: Integer
n = 0xfffffffffffffffffffffffffffffffebaaedce6af48a03bbfd25e8cd0364141

 -- octet size of p
lengthp :: Int
lengthp = 32 -- octets

 -- octet size of n
lengthn :: Int
lengthn = 32 -- octets

newtype Fp = Fp Integer deriving (Eq, Show)

instance Num Fp where
  (Fp a) + (Fp b) | s < p     = Fp s
                  | otherwise = Fp $ s - p
    where
      s = a + b
  negate (Fp 0) = Fp 0
  negate (Fp a) = Fp (p - a)
  (Fp a) * (Fp b) = Fp $ (a * b) `rem` p
  fromInteger a = Fp $ a `mod` p

instance Fractional Fp where
  recip (Fp x) | a*x `mod` p == 1 = Fp a
               | otherwise = error "recip of 0 divisor mod p is undefined"
   where
     (a,b) = xgcdMod p x p
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

sqrtFp :: Fp -> [Fp]
sqrtFp x | sanityCheck = guard (y^2 == x) >> [y, (-y)]
         | otherwise = error "prime p isn't congurent to 3 mod 4"
 where
   (q,r) = quotRem (p+1) 4
   sanityCheck = r == 0
   y = x^q

instance Serialize Fp where
  get = do bs <- getBytes lengthp
           let x = nonNegativeByteStringBE bs
           unless (x < p) (fail $ "not in Fp: "++show x)
           return (Fp x)
  put (Fp a) = putByteString . integerToNByteStringBE lengthp $ a

newtype Fn = Fn Integer deriving (Eq, Show)

instance Num Fn where
  (Fn a) + (Fn b) | s < n     = Fn s
                  | otherwise = Fn $ s - n
    where
      s = a + b
  negate (Fn 0) = Fn 0
  negate (Fn a) = Fn (n - a)
  (Fn a) * (Fn b) = Fn $ (a * b) `rem` n
  fromInteger a = Fn $ a `mod` n

instance Fractional Fn where
  recip (Fn x) | a*x `mod` n == 1 = Fn a
               | otherwise = error $ "recip of 0 divisor mod n is undefined: "++show a++", "++show x
   where
     (a,b) = xgcdMod n x n
  fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

-- Fn is serialized using DER.
instance Serialize Fn where
  get = do 0x02 <- getWord8
           len <- getWord8
           guard (0 < len && len <= 0x7f)
           l <- getBytes (fromIntegral len)
           {- Here openssl deviates from the DER specfication and treat all inputs as unsigned integers
              We have to follow this deviation in bitcoin since bitcoin is defined in terms of openssl. -}
           let i = nonNegativeByteStringBE l
           guard (i < n)
           return (Fn i)
  put (Fn a) | len <= 0x7f = putWord8 0x02 >> putWord8 (fromIntegral len)
                          >> putByteString (pack l)
             | otherwise = error "Serialize Fn value too large"
   where
     len = length l
     l = fixup . reverse . unfoldr f $ a
     f 0 = Nothing
     f x = Just (fromInteger x :: Word8, x `shiftR` 8)
     fixup [] = [0]
     fixup l@(h:_) | h <= 0x7f = l
                   | otherwise = 0:l

data Point = Point Fp Fp Fp

validPoint :: Point -> Bool
validPoint (Point 0 0 0) = False
validPoint (Point x y z) = y^2 == x^3 + {- a*x*z^4 + -} b*z^6

infinity :: Point
infinity = Point 1 1 0

isInfinity :: Point -> Bool
isInfinity (Point _ _ z) = z == 0

mkPoint :: (Monad m) => Fp -> Fp -> m Point
mkPoint x y = do unless (validPoint p) (fail $ "mkPoint: point "++ show p++" not on curve")
                 return p
 where
  p = Point x y 1

getx, gety :: Point -> Maybe Fp
getx (Point _ _ 0) = Nothing
getx (Point x y z) = Just (x/z^2)
gety (Point _ _ 0) = Nothing
gety (Point x y z) = Just (y/z^3)

instance Eq Point where
  (Point x1 y1 z1) == (Point x2 y2 z2) = x1*z2^2 == x2*z1^2 && y1*z2^3 == y2*z1^3

instance Show Point where
  showsPrec d p = fromMaybe (showString "infinity")
                   (do x <- getx p
                       y <- gety p
                       return $ showParen (10 < d) ( showString "mkPoint "
                                                   . showsPrec 11 x
                                                   . showString " "
                                                   . showsPrec 11 y
                                                   ))

instance Monoid Point where
  mempty = infinity
  {- A large chunk of time is spent in mappend, so anything that would speed this up
     could have a big impact -}
  p1@(Point x1 y1 z1) `mappend` p2@(Point x2 y2 z2)
    | z1 == 0   = p2
    | z2 == 0   = p1
    | n == 0 && d == 0 = double p1
    | otherwise = Point x3 y3 z3
    where
     z12 = z1^2
     z13 = z12 * z1
     z22 = z2^2
     z23 = z22 * z2
     y1z23 = y1 * z23
     x1z22 = x1 * z22
     y2z13 = y2 * z13
     x2z12 = x2 * z12
     n = (y2z13 - y1z23)
     d = (x2z12 - x1z22)
     d2 = d^2
     d3 = d2 * d
     x1z22d2 = x1z22 * d2
     x3 = (n^2 - x1z22d2 - x2z12*d2)
     y3 = (n*(x1z22d2 - x3) - y1z23*d3)
     z3 = z1*z2*d
{-
     z12 = z1^2
     z22 = z2^2
     x1z2 = x1*z22
     x2z1 = x2*z12
     xsum = x1z2 + x2z1
     ysum = y1*z22*z2 + y2*z12*z1
     z1z22 = z12*z22
     ysz2 = ysum*z2
     ysz22 = ysz2^2
     s = xsum^2 - (x1*x2 {-+ a*z1z22-})*z1z22
     x3 = s^2 - xsum*ysum^2
     y3 = s*(x1*ysz22 - x3) - y1*ysz22*ysz2
     z3 = z1*ysz2
-}

instance ZModule Point where
  double (Point x y z) = Point x3 y3 z3
   where
    y2 = y^2
    xys2 = 4*x*y2
    s = 3*x^2 {- + a*z^4 -}
    x3 = s^2 - 2*xys2
    y3 = s*(xys2 - x3) - 8*y2^2
    z3 = 2*y*z
  opposite (Point x y z) = Point x (-y) z

instance Serialize Point where
  get = do w <- get
           p <- go (w :: Word8)
           unless (validPoint p) (fail "Point does not lie on curve")
           return p
   where
    go 0 = return mempty
    go 4 = do x <- get
              y <- get
              mkPoint x y
    go w | w /= 2 && w /= 3 = (fail "Invalid Point prefix")
         | otherwise = do
            x <- get
            let py = listToMaybe . filter sameSign $ sqrtFp (x^3 + {- a*x + -} b)
            y <- maybe (fail $ "No Point on curve with x = "++(show x)) return py
            mkPoint x y
     where
      sameSign (Fp a) = even w == even a

  put = putUncompressed

putUncompressed :: Point -> Put
putUncompressed p = fromMaybe (put (0 :: Word8)) (do { x <- getx p; y <- gety p; return (put (4 :: Word8) >> put x >> put y)})

putCompressed :: Point -> Put
putCompressed p = fromMaybe (put (0 :: Word8)) (do { x <- getx p; y <- gety p; return (put (prefix y) >> put x)})
 where
  prefix :: Fp -> Word8
  prefix (Fp y) = 2 .|. ((fromInteger y) .&. 1)

scale :: Fn -> Point -> Point
scale (Fn n) x = x *+ n

{- compute a `scale` p + b `scale` q -}
shamirsTrick :: Fn -> Point -> Fn -> Point -> Point
shamirsTrick (Fn a) p (Fn b) q = go a b
  where
    pq = p <+> q
    go 0 0 = mempty
    go a b | evena && evenb = base2
           | otherwise = base2 <+> additional
      where
        base = go (a `quot` 2) (b `quot` 2)
        base2 = double base
        evena = even a
        evenb = even b
        additional | evena = q
                   | evenb = p
                   | otherwise = pq

-- this crazy function is used in the crypto operations
castFpToFn :: Fp -> Fn
castFpToFn (Fp a) = fromInteger a

-- this crazy function is used in the key recovery crypto operation
uncastFnToFp :: Fn -> [Fp]
uncastFnToFp (Fn a) = map (fromInteger) (takeWhile (< p) [a + i*n | i <- [0..]])

{- -- lpsmith's algorithm
xgcdMod p x y = go (0,y) (1, x)
 where
  go (a,x) (c,0) = (a,c)
  go (a,x) z@(c,y) = c `seq` y `seq` go z ((a - c*q) `mod` p, r)
   where
    (q,r) = quotRem x y
-}
xgcdMod p x 0 = (1,0)
xgcdMod p 0 y = (0,1)
xgcdMod p x y = (b, (a-b*q) `mod` p)
  where
    (q,r) = quotRem x y
    (a,b) = xgcdMod p y r