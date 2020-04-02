{-# LANGUAGE PolyKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Barbies.Internal.DistributiveB
  ( DistributiveB(..)
  , gbdistributeDefault
  , CanDeriveDistributiveB
  , bshape
  )

where

import Barbies.Internal.FunctorB (FunctorB(..))
import Barbies.Generics.Distributive (GDistributive(..))

import Data.Functor.Compose   (Compose (..))
import Data.Functor.Identity  (Identity (..))
-- import Data.Functor.Const     (Const (..))
-- import Data.Functor.Constant  (Constant (..))
import Data.Functor.Product   (Product (..))
import Data.Generics.GenericN
import Data.Proxy             (Proxy (..))
import Data.Distributive
import Data.Kind              (Type)

class (FunctorB b) => DistributiveB (b :: (k -> Type) -> Type) where
  bdistribute :: Distributive f => f (b g) -> b (Compose f g)

  default bdistribute
    :: forall f g
    .  CanDeriveDistributiveB b f g
    => Distributive f => f (b g) -> b (Compose f g)
  bdistribute = gbdistributeDefault

-- Analogous to `bsequence'`
bdistribute' :: (DistributiveB b, Distributive f) => f (b Identity) -> b f
bdistribute' = bmap (fmap runIdentity . getCompose) . bdistribute

bshape :: DistributiveB b => b ((->) (b Identity))
bshape = bdistribute' id

-- reshape :: (DistributiveB b) => b ((->) (b Identity)) -> b Identity -> b Identity
-- reshape sh x = bmap (\f -> Identity (f x)) sh

type CanDeriveDistributiveB b f g
  = ( GenericP 0 (b g)
    , GenericP 0 (b (Compose f g))
    , GDistributive 0 f (RepP 0 (b g)) (RepP 0 (b (Compose f g)))
    )

-- | Default implementation of 'bdistribute' based on 'Generic'.
gbdistributeDefault
  :: CanDeriveDistributiveB b f g
  => Distributive f => f (b g) -> b (Compose f g)
gbdistributeDefault
  = toP (Proxy @0) . gdistribute (Proxy @0) . fmap (fromP (Proxy @0))
{-# INLINE gbdistributeDefault #-}

-- --------------------------------
-- Instances for base types
-- --------------------------------

instance DistributiveB Proxy where
  bdistribute _ = Proxy
  {-# INLINE bdistribute #-}

fstF :: Product f g a -> f a
fstF (Pair x _y) = x

sndF :: Product f g a -> g a
sndF (Pair _x y) = y

instance (DistributiveB a, DistributiveB b) => DistributiveB (Product a b) where
  bdistribute xy = Pair (bdistribute $ fstF <$> xy) (bdistribute $ sndF <$> xy)
  {-# INLINE bdistribute #-}

-- instance FunctorB (Const x) where
--   bmap _ (Const x) = Const x
--   {-# INLINE bmap #-}

-- instance (Functor f, FunctorB b) => FunctorB (f `Compose` b) where
--   bmap h (Compose x) = Compose (bmap h <$> x)
--   {-# INLINE bmap #-}


-- -- --------------------------------
-- -- Instances for transformer types
-- -- --------------------------------

-- instance FunctorB (Constant x) where
--   bmap _ (Constant x) = Constant x
--   {-# INLINE bmap #-}
