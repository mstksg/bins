{-# LANGUAGE ApplicativeDo                            #-}
{-# LANGUAGE DataKinds                                #-}
{-# LANGUAGE DeriveFunctor                            #-}
{-# LANGUAGE ExistentialQuantification                #-}
{-# LANGUAGE FlexibleContexts                         #-}
{-# LANGUAGE KindSignatures                           #-}
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
    BinSpec(..), linBS, logBS, gaussBS
  -- ** 'BinView'
  , BinView, binView, linView, logView, gaussView
  -- ** Inspecting 'BinSpec'
  , binSpecIntervals
  -- * Creating and manipulating bins
  , Bin, Binner, withBinner, fromFin
  -- ** Inspecting bins
  , binFin, binRange, binMin, binMax
  , binFinExt, binFinComp
  -- ** Showing bins
  , displayBin, displayBinDouble
  -- *** In-depth inspection
  , Pointed(..), pointed, pElem, binIx, fromIx, expandFin, unexpandFin
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
import           Numeric.SpecFunctions
import           Text.Printf
import           Unsafe.Coerce
import qualified Data.Map              as M
import qualified Data.Vector.Sized     as SV

-- | A bidirectional "view" to transform the data type before binning.
--
-- See 'linView' for a linear binning, and 'logView' for a logarithmic
-- binning.  You can construct your own custom transformer using 'binView'.
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

-- | Binning based on a Gaussian Distribution.  Bins "by standard
-- deviation"; there are more bins the closer to the mean you get, and less
-- bins the farther away.
gaussView
    :: RealFrac a
    => a           -- ^ center / mean
    -> a           -- ^ standard deviation
    -> BinView a Double
gaussView μ σ = binView to from
  where
    to   = erf . realToFrac . (/ σ) . subtract μ
    from = (+ μ) . (* σ) . realToFrac . invErf

view :: BinView a b -> a -> b
view v = runForget (v (Forget id))

review :: BinView a b -> b -> a
review v = unTagged . v . Tagged

-- | Specification of binning.
--
-- A @'BinSpec' n a b@ will bin values of type @a@ into @n@ bins, according
-- to a scaling in type @b@.
--
-- Constructor is meant to be used with type application syntax to indicate
-- @n@, like @'BinSpec' @5 0 10 'linView'@
data BinSpec (n :: Nat) a b =
        BS { bsMin  :: a             -- ^ lower bound of values
           , bsMax  :: a             -- ^ upper bound of values
           , bsView :: BinView a b   -- ^ binning view
           }

-- | Convenient constructor for a 'BinSpec' for a linear scaling.
--
-- Meant to be used with type application syntax:
--
-- @
-- 'linBS' @5 0 10
-- @
linBS
    :: forall n a. ()
    => a                            -- ^ Lower bound
    -> a                            -- ^ Upper bound
    -> BinSpec n a a
linBS mn mx = BS mn mx linView

-- | Convenient constructor for a 'BinSpec' for a logarithmic scaling.
--
-- Meant to be used with type application syntax:
--
-- @
-- 'logBS' @5 0 10
-- @
logBS
    :: forall n a. Floating a
    => a                            -- ^ Lower bound
    -> a                            -- ^ Upper bound
    -> BinSpec n a a
logBS mn mx = BS mn mx logView

-- | Convenient constructor for a 'BinSpec' for a gaussian scaling.  Uses
-- the midpoint as the inferred mean.
--
-- Meant to be used with type application syntax:
--
-- @
-- 'gaussBS' @5 3 0 10
-- @
--
-- indicates that you want 5 bins.
gaussBS
    :: forall n a. RealFrac a
    => a                            -- ^ Standard Deviation
    -> a                            -- ^ Lower bound
    -> a                            -- ^ Upper bound
    -> BinSpec n a Double
gaussBS σ mn mx = BS mn mx (gaussView ((mn + mx)/2) σ)

-- | Data type extending a value with an extra "minimum" and "maximum"
-- value.
data Pointed a = Bot
               | PElem !a
               | Top
  deriving (Show, Eq, Ord, Functor)

-- | Church-style deconstructor for 'Pointed', analogous to 'maybe',
-- 'either', and 'bool'.
--
-- @since 0.1.1.0
pointed
    :: b                -- ^ return if 'Bot'
    -> (a -> b)         -- ^ apply if 'PElem'
    -> b                -- ^ return if 'Top'
    -> Pointed a
    -> b
pointed bot pelem top = \case
    Bot     -> bot
    PElem x -> pelem x
    Top     -> top

-- | Extract the item from a 'Pointed' if it is neither the extra minimum
-- or maximum.
pElem :: Pointed a -> Maybe a
pElem = pointed Nothing Just Nothing

-- | A @'Bin' s n@ is a single bin index out of @n@ partitions of the
-- original data set, according to a 'BinSpec' represented by @s@.
--
-- All 'Bin's with the same @s@ follow the same 'BinSpec', so you can
-- safely use 'binRange' 'withBinner'.
--
-- It has useful 'Eq' and 'Ord' instances.
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
-- value was too high or too low.  Also see 'binFinExt', which extends the
-- range of the 'Finite' to embed lower or higher values.
binFin :: Bin s n -> Maybe (Finite n)
binFin = pElem . binIx

-- | Like 'binFin', but return the true "n + 2" slot number of a 'Bin',
-- where 'minBound' is "below minimum" and 'maxBound' is "above maximum"
--
-- @since 0.1.1.0
binFinExt
    :: KnownNat n
    => Bin s n
    -> Finite (1 + n + 1)
binFinExt = expandFin . binIx

-- | Like 'binFin', but squishes or compresses "below minimum" to "above
-- maximum" bins into the 'Finite', counting them in the same bin as the
-- minimum and maximum bin, respectively.
--
-- @since 0.1.1.0
binFinComp
    :: KnownNat n
    => Bin s n
    -> Finite n
binFinComp = pointed minBound id maxBound . binIx

tick
    :: forall n a b. (KnownNat n, Fractional b)
    => BinSpec n a b
    -> b
tick BS{..} = totRange / fromIntegral (natVal (Proxy @n))
  where
    totRange = view bsView bsMax - view bsView bsMin

packPointed
    :: KnownNat n
    => Integer
    -> Pointed (Finite n)
packPointed n
    | n < 0     = Bot
    | otherwise = maybe Top PElem . packFinite $ n

mkBin_
    :: forall n a b. (KnownNat n, RealFrac b)
    => BinSpec n a b
    -> a
    -> Pointed (Finite n)
mkBin_ bs = packPointed
          . floor
          . (/ tick bs)
          . subtract (scaleIn (bsMin bs))
          . scaleIn
  where
    scaleIn = view (bsView bs)

mkBin
    :: forall n a b s. (KnownNat n, RealFrac b, Reifies s (BinSpec n a b))
    => a
    -> Bin s n
mkBin = Bin . mkBin_ (reflect (Proxy @s))

-- | The type of a "binning function", given by 'withBinner'.  See
-- 'withBinner' for information on how to use.
type Binner s n a = a -> Bin s n

-- | With a 'BinSpec', give a "binning function" that you can use to create
-- bins within a continuation.
--
-- @
-- 'withBinner' myBinSpec $ \toBin ->
--     show (toBin 2.8523)
-- @
--
-- Uses a Rank-N continution to ensure that you can only compare 'Bin's
-- constructed from the same 'BinSpec'/binning function.
withBinner
    :: (KnownNat n, RealFrac b)
    => BinSpec n a b
    -> (forall s. Reifies s (BinSpec n a b) => Binner s n a -> r)
    -> r
withBinner bs f = reify bs $ \(_ :: Proxy s) -> f @s mkBin

-- | Generate a vector of the boundaries deriving the bins from
-- a 'BinSpec'.  Can be useful for debugging.
binSpecIntervals
    :: forall n a b. (KnownNat n, Fractional b)
    => BinSpec n a b
    -> SV.Vector (n + 1) a
binSpecIntervals bs = SV.generate $ \i ->
    case strengthen i of
      Just (fromIntegral->i') -> scaleOut $ i' * t + scaleIn (bsMin bs)
      Nothing                 -> bsMax bs
  where
    t        = tick bs
    scaleIn  = view (bsView bs)
    scaleOut = review (bsView bs)

binRange_
    :: forall n a b. (KnownNat n, Fractional b)
    => BinSpec n a b
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
-- the 'BinSpec', so is "unbounded" in that direction.
binRange
    :: forall n a b s. (KnownNat n, Fractional b, Reifies s (BinSpec n a b))
    => Bin s n
    -> (Maybe a, Maybe a)
binRange = binRange_ (reflect (Proxy @s)) . binIx

-- | Extract the minimum of the range indicabed by a given 'Bin'.
--
-- A 'Nothing' value means that the original value was below the minimum
-- limit of the 'BinSpec', so is "unbounded" in the lower direction.
binMin
    :: forall n a b s. (KnownNat n, Fractional b, Reifies s (BinSpec n a b))
    => Bin s n
    -> Maybe a
binMin = fst . binRange

-- | Extract the maximum of the range indicabed by a given 'Bin'.
--
-- A 'Nothing' value means that the original value was above the maximum
-- limit of the 'BinSpec', so is "unbounded" in the upper direction.
binMax
    :: forall n a b s. (KnownNat n, Fractional b, Reifies s (BinSpec n a b))
    => Bin s n
    -> Maybe a
binMax = snd . binRange

-- | Display the interval maintained by a 'Bin'.
displayBin
    :: forall n a b s. (KnownNat n, Fractional b, Reifies s (BinSpec n a b))
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

-- | Display the interval maintained by a 'Bin', if the 'Bin' contains
-- a 'Double'.
displayBinDouble
    :: forall n b s. (KnownNat n, Fractional b, Reifies s (BinSpec n Double b))
    => Int                      -- ^ number of decimal places to round
    -> Bin s n
    -> String
displayBinDouble d = displayBin (printf ("%." ++ show d ++ "f"))

instance (KnownNat n, Show a, Fractional b, Reifies s (BinSpec n a b)) => Show (Bin s n) where
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
-- main = withBinner (logBS @10 5 50) $ \toBin ->
--     mapM_ (\(b, n) -> putStrLn (displayBinDouble 4 b ++ "\t" ++ show n))
--   . M.toList
--   $ binFreq toBin xs
-- @
--
-- @
-- (-inf .. 5.0000)        4
-- [5.0000 .. 6.2946)      2
-- [6.2946 .. 7.9245)      1
-- [7.9245 .. 9.9763)      2
-- [9.9763 .. 12.5594)     3
-- [12.5594 .. 15.8114)    3
-- [15.8114 .. 19.9054)    4
-- [19.9054 .. 25.0594)    6
-- [25.0594 .. 31.5479)    6
-- [31.5479 .. 39.7164)    8
-- [39.7164 .. 50.0000)    10
-- [50.0000 .. +inf)       51
-- @
binFreq
    :: forall n t a s. Foldable t
    => Binner s n a
    -> t a
    -> M.Map (Bin s n) Int
binFreq toBin = M.unionsWith (+) . map go . toList
  where
    go :: a -> M.Map (Bin s n) Int
    go x = M.singleton (toBin x) 1

-- | Construct a 'Bin' if you know the bin number you want to specify, or
-- if the bin is over or under the maximum.
fromIx :: Pointed (Finite n) -> Bin s n
fromIx = Bin

-- | Construct a 'Bin' if you know the bin number you want to specify.  See
-- 'fromIx' if you want to specify bins that are over or under the maximum,
-- as well.
fromFin :: Finite n -> Bin s n
fromFin = fromIx . PElem

-- | "Expand" a 'Pointed' containing a 'Finite' to a wider-ranged 'Finite'.
-- Used for 'binFinExt'
--
-- @since 0.1.2.0
expandFin :: KnownNat n => Pointed (Finite n) -> Finite (1 + n + 1)
expandFin = pointed minBound (weaken . shift) maxBound

-- | The inverse of 'expandFin': "re-pack" a 'Finite' back into
-- a 'Pointed' containing a narrower-ranged 'Finite'.
--
-- @since 0.1.2.0
unexpandFin :: KnownNat n => Finite (1 + n + 1) -> Pointed (Finite n)
unexpandFin x = case unshift x of
  Nothing -> Bot
  Just y  -> case strengthen y of
    Nothing -> Top
    Just z  -> PElem z


-- | A @'SomeBin' a n@ is @'Bin' s n@, except with the 'BinSpec' s hidden.
-- It's useful for returning out of 'withBinner'.
--
-- It has useful 'Eq' and 'Ord' instances.
--
-- To be able to "unify" two 'Bin's inside a 'SomeBin', use 'sameBinSpec'
-- to verify that the two 'SomeBin's were created with the same 'BinSpec'.
data SomeBin a n = forall s b. (Fractional b, Reifies s (BinSpec n a b))
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
-- allowing you to use functions like '==' and 'compare' on 'Bin's that you
-- get out of a 'SomeBin'.
sameBinSpec
    :: forall s t n a b p. (Reifies s (BinSpec n a b), Reifies t (BinSpec n a b), KnownNat n, Eq a, Fractional b)
    => p s
    -> p t
    -> Maybe (s :~: t)
sameBinSpec _ _ = do
    guard $ binSpecIntervals bs1 == binSpecIntervals bs2
    pure (unsafeCoerce Refl)
  where
    bs1 = reflect (Proxy @s)
    bs2 = reflect (Proxy @t)
