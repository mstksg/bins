{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ViewPatterns        #-}

module Data.Bin (
    BinView, linView, logView
  , BinSpec(..), linBS, logBS
  , Bin, binFin, Binner, withBinner
  , binRange, binMin, binMax
  , Pointed(..), pElem, binIx
  ) where

import           Data.Finite
import           Data.Profunctor
import           Data.Proxy
import           Data.Reflection
import           Data.Tagged
import           GHC.TypeNats
import           Numeric.Natural

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
--     toBin @5 2.8523      -- split into five bins
-- @
withBinner
    :: RealFrac b
    => BinSpec a b
    -> (forall s. Binner s a -> r)
    -> r
withBinner bs f = reify bs $ \(_ :: Proxy s) -> f @s mkBin

binRange_
    :: forall n a b. (KnownNat n, Fractional b)
    => BinSpec a b
    -> Pointed (Finite n)
    -> (Maybe a, Maybe a)
binRange_ bs = \case
    Bot                     -> ( Nothing      , Just minRange)
    PElem (fromIntegral->i) -> ( Just (scaleOut ( i      * t))
                               , Just (scaleOut ((i + 1) * t))
                               )
    Top                     -> ( Just maxRange, Nothing      )
  where
    minRange = scaleOut 0
    maxRange = scaleOut . fromIntegral . natVal $ Proxy @n
    t        = tick bs (natVal (Proxy @n))
    scaleOut = review (bsView bs)

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
