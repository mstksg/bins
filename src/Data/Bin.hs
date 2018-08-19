{-# LANGUAGE ApplicativeDo                            #-}
{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveFunctor                            #-}
{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE LambdaCase                               #-}
{-# LANGUAGE RankNTypes                               #-}
{-# LANGUAGE RecordWildCards                          #-}
{-# LANGUAGE ScopedTypeVariables                      #-}
{-# LANGUAGE StandaloneDeriving                       #-}
{-# LANGUAGE TypeApplications                         #-}
{-# LANGUAGE TypeOperators                            #-}
{-# LANGUAGE UndecidableInstances                     #-}
{-# LANGUAGE ViewPatterns                             #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise       #-}

-- |
-- Module      : Data.Bin
-- Copyright   : (c) Justin Le 2018
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Tools for aggregating numeric values into a set of discrete bins
-- according to some binning specification.
--
-- See 'withBinner' for main usage information, and 'Bin' for the main
-- binned data type, and 'binFreq' for a common usage example.
--

module Data.Bin (
  -- * Specifying the binning
    BinView, linView, logView
  , BinSpec(..), linBS, logBS
  -- * Creating and manipulating bins
  , Bin, Binner, withBinner
  -- ** Inspecting bins
  , binFin, binRange, binMin, binMax
  -- ** Showing bins
  , displayBin, displayBinDouble
  -- *** In-depth inspection
  , Pointed(..), pElem, binIx
  -- * Untyped
  , SomeBin(..), sameBinSpec
  -- * Handy use patterns
  , binFreq
  ) where

import           Control.Monad
import           Data.Finite
import           Data.Foldable
import           Data.Profunctor
import           Data.Proxy
import           Data.Reflection
import           Data.Tagged
import           Data.Type.Equality
import           GHC.TypeNats
import           Numeric.Natural
import           Text.Printf
import           Unsafe.Coerce
import qualified Data.Map           as M
import qualified Data.Vector.Sized  as SV

-- | A bidirectional "view" to transform the data type before binning.
--
-- See 'linView' for a linear binning, and 'logView' for a logarithmic
-- binning.
--
-- This type is essentially 'Iso' from the /lens/ library, and any 'Iso''
-- from lens can be used here.  However, it is important that all of these
-- represent /monotonic/ isomorphisms.
type BinView a b = forall p. Profunctor p => p b b -> p a a

-- | Construct a 'BinView' based on "to" and "from" functions
--
-- It is important that the "to" and "from" functions be /inverses/ of each
-- other.  Furthermore, both "to" and "from" should be __monotonic__.
binView
    :: (a -> b)       -- ^ "to"
    -> (b -> a)       -- ^ "from"
    -> BinView a b
binView = dimap

-- | Linear binning
linView :: BinView a a
linView = binView id id

-- | Logarithmic binning (smaller bins at lower levels, larger bins at
-- higher levels).
logView :: Floating a => BinView a a
logView = binView log exp

view :: BinView a b -> a -> b
view v = runForget (v (Forget id))

review :: BinView a b -> b -> a
review v = unTagged . v . Tagged

-- | Specification of binning.
--
-- A @'BinSpec' a b@ will bin values of type @a@, according to a scaling in
-- type @b@.
data BinSpec a b = BS { bsMin  :: a             -- ^ lower bound of values
                      , bsMax  :: a             -- ^ upper bound of values
                      , bsView :: BinView a b   -- ^ binning view
                      }

-- | Convenient constructor for a 'BinSpec' for a linear scaling.
linBS :: a -> a -> BinSpec a a
linBS mn mx = BS mn mx linView

-- | Convenient constructor for a 'BinSpec' for a logarithmic scaling.
logBS :: Floating a => a -> a -> BinSpec a a
logBS mn mx = BS mn mx logView

-- | Data type extending a value with an extra "minimum" and "maximum"
-- value.
data Pointed a = Bot
               | PElem !a
               | Top
  deriving (Show, Eq, Ord, Functor)

-- | Extract the item from a 'Pointed' if it is neither the extra minimum
-- or maximum.
pElem :: Pointed a -> Maybe a
pElem = \case
    Bot     -> Nothing
    PElem x -> Just x
    Top     -> Nothing

-- | A @'Bin' s n@ is a single bin index out of @n@ partitions of the
-- original data set, according to a 'BinSpec' represented by @s@.
--
-- All 'Bin's with the same @s@ follow the same 'BinSpec', so you can
-- safely use 'binRange' 'withBinner'.
--
-- Actually has @n + 2@ partitions, since it also distinguishes values
-- that are outside the 'BinSpec' range.
newtype Bin s n = Bin { _binIx :: Pointed (Finite n) }
  deriving (Eq, Ord)

-- | A more specific version of 'binFin' that indicates whether or not the
-- value was too high or too low for the 'BinSpec' range.
binIx :: Bin s n -> Pointed (Finite n)
binIx = _binIx

-- | Extract, potentially, the 'Bin' index.  Will return 'Nothing' if the
-- original value was outside the 'BinSpec' range.
--
-- See 'binIx' for a more specific version, which indicates if the original
-- value was too high or too low.
binFin :: Bin s n -> Maybe (Finite n)
binFin = pElem . binIx

tick :: Fractional b => BinSpec a b -> Natural -> b
tick BS{..} n = totRange / fromIntegral n
  where
    totRange = view bsView bsMax - view bsView bsMin

packExtFinite
    :: KnownNat n
    => Integer
    -> Pointed (Finite n)
packExtFinite n
    | n < 0     = Bot
    | otherwise = maybe Top PElem . packFinite $ n

mkBin_
    :: forall n a b. (KnownNat n, RealFrac b)
    => BinSpec a b
    -> a
    -> Pointed (Finite n)
mkBin_ bs = packExtFinite
          . round
          . (/ tick bs (natVal (Proxy @n)))
          . subtract (scaleIn (bsMin bs))
          . scaleIn
  where
    scaleIn = view (bsView bs)

mkBin
    :: forall n a b s. (KnownNat n, RealFrac b, Reifies s (BinSpec a b))
    => a
    -> Bin s n
mkBin = Bin . mkBin_ (reflect (Proxy @s))

-- | The type of a "binning function", given by 'withBinner'.  See
-- 'withBinner' for information on how to use.
type Binner s a = forall n. KnownNat n => a -> Bin s n

-- | With a 'BinSpec', give a "binning function" that you can use to create
-- bins within a continuation.  The binning function is meant to be used
-- with TypeApplications to specify how many bins to use:
--
-- @
-- 'withBinner' myBinSpec $ \toBin ->
--     toBin @5 2.8523      -- assign to one of five bins
-- @
withBinner
    :: RealFrac b
    => BinSpec a b
    -> (forall s. Reifies s (BinSpec a b) => Binner s a -> r)
    -> r
withBinner bs f = reify bs $ \(_ :: Proxy s) -> f @s mkBin

binSpecIntervals
    :: forall n a b. (KnownNat n, Fractional b)
    => BinSpec a b
    -> SV.Vector (n + 1) a
binSpecIntervals bs = SV.generate $ \i ->
    case strengthen i of
      Just (fromIntegral->i') -> scaleOut $ i' * t + scaleIn (bsMin bs)
      Nothing                 -> bsMax bs
  where
    t        = tick bs (natVal (Proxy @n))
    scaleIn  = view (bsView bs)
    scaleOut = review (bsView bs)

binRange_
    :: forall n a b. (KnownNat n, Fractional b)
    => BinSpec a b
    -> Pointed (Finite n)
    -> (Maybe a, Maybe a)
binRange_ bs = \case
    Bot     -> ( Nothing         , Just (SV.head v))
    PElem i -> ( Just (v `SV.index` weaken i)
               , Just (v `SV.index` shift i )
               )
    Top     -> ( Just (SV.last v), Nothing         )
  where
    v        = binSpecIntervals @n bs

-- | Extract the minimum and maximum of the range indicabed by a given
-- 'Bin'.
--
-- A 'Nothing' value indicates that we are outside of the normal range of
-- the 'BinSpec', so is "unbounded".
binRange
    :: forall n a b s. (KnownNat n, Fractional b, Reifies s (BinSpec a b))
    => Bin s n
    -> (Maybe a, Maybe a)
binRange = binRange_ (reflect (Proxy @s)) . binIx

-- | Extract the minimum of the range indicabed by a given 'Bin'.
--
-- A 'Nothing' value means that the original value was below the minimum
-- limit of the 'BinSpec', so is "unbounded".
binMin
    :: forall n a b s. (KnownNat n, Fractional b, Reifies s (BinSpec a b))
    => Bin s n
    -> Maybe a
binMin = fst . binRange

-- | Extract the maximum of the range indicabed by a given 'Bin'.
--
-- A 'Nothing' value means that the original value was above the maximum
-- limit of the 'BinSpec', so is "unbounded".
binMax
    :: forall n a b s. (KnownNat n, Fractional b, Reifies s (BinSpec a b))
    => Bin s n
    -> Maybe a
binMax = snd . binRange

displayBin
    :: forall n a b s. (KnownNat n, Fractional b, Reifies s (BinSpec a b))
    => (a -> String)        -- ^ how to display a value
    -> Bin s n
    -> String
displayBin f b = printf "%s .. %s" mn' mx'
  where
    (mn, mx) = binRange b
    mn' = case mn of
            Nothing -> "(-inf"
            Just m  -> "[" ++ f m
    mx' = case mx of
            Nothing -> "+inf)"
            Just m  -> f m ++ ")"

displayBinDouble
    :: forall n b s. (KnownNat n, Fractional b, Reifies s (BinSpec Double b))
    => Int                      -- ^ number of decimal places to round
    -> Bin s n
    -> String
displayBinDouble d = displayBin (printf ("%." ++ show d ++ "f"))

instance (KnownNat n, Show a, Fractional b, Reifies s (BinSpec a b)) => Show (Bin s n) where
    showsPrec d b = showParen (d > 10) $
      showString "Bin " . showString (displayBin @n show b)

-- | Generate a histogram: given a container of @a@s, generate a frequency
-- map of how often values in a given discrete bin occurred.
--
-- @
-- xs :: [Double]
-- xs = [1..100]
--
-- main :: IO ()
-- main = withBinner (logBS 5 50) $ \toBin ->
--     mapM_ (\(b, n) -> putStrLn (displayBin 4 b ++ "\t" ++ show n))
--   . M.toList
--   $ binFreq @10 toBin xs
-- @
--
-- @
-- (-inf .. 5.0000)        4
-- [5.0000 .. 6.2946)      1
-- [6.2946 .. 7.9245)      2
-- [7.9245 .. 9.9763)      1
-- [9.9763 .. 12.5594)     3
-- [12.5594 .. 15.8114)    3
-- [15.8114 .. 19.9054)    3
-- [19.9054 .. 25.0594)    5
-- [25.0594 .. 31.5479)    6
-- [31.5479 .. 39.7164)    7
-- [39.7164 .. 50.0000)    9
-- [50.0000 .. +inf)       56
-- @
binFreq
    :: forall n t a s. (KnownNat n, Foldable t)
    => Binner s a
    -> t a
    -> M.Map (Bin s n) Int
binFreq toBin = M.unionsWith (+) . map go . toList
  where
    go :: a -> M.Map (Bin s n) Int
    go x = M.singleton (toBin x) 1

-- | A @'Bin' s n@ is a single bin index out of @n@ partitions of the
-- original data set, according to a 'BinSpec' represented by @s@.
--
-- All 'Bin's with the same @s@ follow the same 'BinSpec', so you can
-- safely use 'binRange' 'withBinner'.
--
-- Actually has @n + 2@ partitions, since it also distinguishes values
-- that are outside the 'BinSpec' range.
data SomeBin a n = forall s b. (Fractional b, Reifies s (BinSpec a b)) 
    => SomeBin { getSomeBin :: Bin s n }

deriving instance (KnownNat n, Show a) => Show (SomeBin a n)

-- | Compares if the ranges match.  Note that this is less performant than
-- comparing the original 'Bin's, or extracting and using 'sameBinSpec'.
instance (KnownNat n, Eq a) => Eq (SomeBin a n) where
    SomeBin x == SomeBin y = binRange x == binRange y

-- | Lexicographical ordering -- compares the lower bound, then the upper
-- bounds.  Note that this is less performant than comparing the original
-- 'Bin's, or extracting and using 'sameBinSpec'
instance (KnownNat n, Ord a) => Ord (SomeBin a n) where
    compare (SomeBin x) (SomeBin y) = compare (binRange x) (binRange y)

-- | Verify that the two reified 'BinSpec' types refer to the same one,
-- allowing you to use functions like '==' and 'compare' on 'Bin's.
sameBinSpec
    :: forall s t a b p. (Reifies s (BinSpec a b), Reifies t (BinSpec a b), Eq a, Fractional b)
    => p s
    -> p t
    -> Maybe (s :~: t)
sameBinSpec _ _ = do
    guard $ binSpecIntervals @3 bs1 == binSpecIntervals @3 bs2
    pure (unsafeCoerce Refl)
  where
    bs1 = reflect (Proxy @s)
    bs2 = reflect (Proxy @t)
