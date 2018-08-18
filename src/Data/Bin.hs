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
  , BinSpec(..)
  , Bin, binIx, binFin, Binner, withBinner
  , binRange, binMin, binMax
  , Pointed(..), pElem
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
type BinView a b = forall p. Profunctor p => p b b -> p a a

-- | Construct a 'BinView' based on "to" and "from" functions
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

-- | Specification of binning
data BinSpec a b = BS { bsMin  :: a             -- ^ lower bound of values
                      , bsMax  :: a             -- ^ upper bound of values
                      , bsView :: BinView a b   -- ^ binning view
                      }

data Pointed a = Bot
               | PElem !a
               | Top
  deriving (Show, Eq, Ord, Functor)

pElem :: Pointed a -> Maybe a
pElem = \case
    Bot     -> Nothing
    PElem x -> Just x
    Top     -> Nothing

-- | A @'Bin' s n@ is a single bin index out of @n@ partitions of the
-- original data set.  See 'binIx' to get the raw index.
newtype Bin s n = Bin { _binIx :: Pointed (Finite n) }

binIx :: Bin s n -> Pointed (Finite n)
binIx = _binIx

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

type Binner s a = forall n. KnownNat n => a -> Bin s n

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

binRange
    :: forall n a b s. (KnownNat n, Fractional b, Reifies s (BinSpec a b))
    => Bin s n
    -> (Maybe a, Maybe a)
binRange = binRange_ (reflect (Proxy @s)) . binIx

binMin, binMax
    :: forall n a b s. (KnownNat n, Fractional b, Reifies s (BinSpec a b))
    => Bin s n
    -> Maybe a
binMin = fst . binRange
binMax = snd . binRange
