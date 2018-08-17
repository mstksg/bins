{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Data.Bin (
    BinView, BinSpec(..)
  , Bin, Binner, withBinner
  , mkBin
  ) where

import           Data.Finite
import           Data.Maybe
import           Data.Profunctor
import           Data.Proxy
import           Data.Reflection
import           Data.Tagged
import           GHC.TypeNats
import           Numeric.Natural

type BinView a b = forall p. Profunctor p => p b b -> p a a

view :: BinView a b -> a -> b
view v = runForget (v (Forget id))

review :: BinView a b -> b -> a
review v = unTagged . v . Tagged

data BinSpec a b = BS { bsMin  :: a
                      , bsMax  :: a
                      , bsView :: BinView a b
                      }

newtype Bin s n = Bin { binIx :: Finite n }

tick :: Fractional b => BinSpec a b -> Natural -> b
tick BS{..} n = totRange / fromIntegral n
  where
    totRange = scaleIn bsMax - scaleIn bsMin
    scaleIn  = view bsView

mkBin_
    :: forall n a b. (KnownNat n, RealFrac b)
    => BinSpec a b
    -> a
    -> Finite n
mkBin_ bs = fromMaybe maxBound
          . packFinite
          . max 0
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

