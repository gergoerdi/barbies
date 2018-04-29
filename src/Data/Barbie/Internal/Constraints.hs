{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Barbie.Internal.Constraints
  ( ProofOf(..)
  , proof
  , requiringProof

  , ConstraintsB(..)

  , CanDeriveGenericInstance
  , ConstraintsOfMatchesGenericDeriv
  , GConstraintsOf
  , GAdjProof
  , gadjProofDefault
  )

where

import Data.Barbie.Internal.Functor(FunctorB(..))
import Data.Barbie.Internal.Generics

import Data.Constraint(Dict(..), withDict)
import Data.Functor.Classes(Show1(..))
import Data.Functor.Product(Product(..))
import Data.Kind(Constraint)
import Data.Proxy

import GHC.Generics


-- | @'ProofOf' c f a@ is evidence that there exists an instance
--   of @c (f a)@.
newtype ProofOf c f a
  = Proof { proofDict :: Dict (c (f a)) }
  deriving(Eq, Show)

instance Show1 (ProofOf c f) where
  liftShowsPrec _ _ = showsPrec

-- | Build proof of instance.
proof :: c (f a) => ProofOf c f a
proof
  = Proof Dict

-- | Turn a constrained-function into an unconstrained one
--   that demands proof-of-instance instead.
requiringProof :: (c (f a) => r) -> (ProofOf c f a -> r)
requiringProof f
  = \p -> withDict (proofDict p) f


-- | Example definition:
  --
  -- @
  -- data T f = A (f 'Int') (f 'String') | B (f 'Bool') (f 'Int')
  --
  -- instance 'ConstraintsB' T where
  --   type 'ConstraintsOf' c f T = (c (f 'Int'), c (f 'String'), c (f 'Bool'))
  --
  --   adjProof t = case t of
  --     A x y -> A ('Pair' 'proof' x) ('Pair' 'proof' y)
  --     B x y -> B ('Pair' 'proof' x) ('Pair' 'proof' y)
  -- @
  --
  -- There are default implementation of 'ConstraintsOf' and 'adjProof' for
  -- 'Generic' types, so in practice one can simply do:
  --
  -- @
  -- derive instance 'Generic' T
  -- instance 'ConstraintsB' T
  -- @
class FunctorB b => ConstraintsB b where
  -- | @'ConstraintsOf' c f b@ should contain a constraint @c (f x)@
  --  for each @f x@ occurring in @b@. E.g.:
  --
  -- @
  -- 'ConstraintsOf' 'Show' f Barbie = ('Show' (f 'String'), 'Show' (f 'Int'))
  -- @
  type ConstraintsOf (c :: * -> Constraint) (f :: * -> *) b :: Constraint
  type ConstraintsOf c f b = GConstraintsOf c f (RecRep (b (Target F)))

  -- | Adjoint a proof-of-instance to a barbie-type.
  adjProof :: ConstraintsOf c f b => b f -> b (Product (ProofOf c f) f)

  default adjProof
    :: ( CanDeriveGenericInstance b
       , ConstraintsOfMatchesGenericDeriv c f b
       , ConstraintsOf c f b
       )
     => b f -> b (Product (ProofOf c f) f)
  adjProof = gadjProofDefault

-- | Intuivively, the requirements to have @'ConstraintsB' B@ derived are:
--
--     * There is an instance of @'Generic' (B f)@ for every @f@
--
--     * If @f@ is used as argument to some type in the definition of @B@, it
--       is only on a Barbie-type with a 'ConstraintsB' instance.
type CanDeriveGenericInstance b
  = ( Generic (b (Target F))
    , Generic (b (Target PxF))
    , GAdjProof b (RecRep (b (Target F)))
    , Rep (b (Target PxF)) ~ Repl' (Target F) (Target PxF) (RecRep (b (Target F)))
    )

type ConstraintsOfMatchesGenericDeriv c f b
  = ConstraintsOf c f b ~ GConstraintsOf c f (RecRep (b (Target F)))


-- ===============================================================
--  Generic derivations
-- ===============================================================

-- NB. Duplicated wrt the definition in 'ProofB' since we don't
-- want to export the 'F' constructor for type-safety.
type family GConstraintsOf (c :: * -> Constraint) (f :: * -> *) r :: Constraint where
  GConstraintsOf c f (M1 _i _c x) = GConstraintsOf c f x
  GConstraintsOf c f V1 = ()
  GConstraintsOf c f U1 = ()
  GConstraintsOf c f (l :*: r) = (GConstraintsOf c f l, GConstraintsOf c f r)
  GConstraintsOf c f (l :+: r) = (GConstraintsOf c f l, GConstraintsOf c f r)
  GConstraintsOf c f (K1 R (NonRec (Target F a))) = c (f a)
  GConstraintsOf c f (K1 R (NonRec (b (Target F)))) = ConstraintsOf c f b
  GConstraintsOf c f (K1 R (RecUsage (b (Target F)))) = () -- break recursion
  GConstraintsOf c f (K1 _i _c) = ()


-- | Default implementation of 'adjProof' based on 'Generic'.
gadjProofDefault
  :: forall b c f
  . ( CanDeriveGenericInstance b
    , ConstraintsOfMatchesGenericDeriv c f b
    , ConstraintsOf c f b
    )
  => b f -> b (Product (ProofOf c f) f)
gadjProofDefault b
  = unsafeUntargetBarbie @PxF $ to $
      gadjProof pcbf $ fromWithRecAnn (unsafeTargetBarbie @F b)
  where
    pcbf = Proxy :: Proxy (c (b f))


data F a
data PxF a

class GAdjProof b rep where
  gadjProof
    :: ( GConstraintsOf c f rep
       , GConstraintsOf c f (RecRep (b (Target F))) -- for the recursive case!
       )
    => Proxy (c (b f)) -> rep x -> Repl' (Target F) (Target PxF) rep x


-- ----------------------------------
-- Trivial cases
-- ----------------------------------

instance GAdjProof b x => GAdjProof b (M1 _i _c x) where
  {-# INLINE gadjProof #-}
  gadjProof pcbf (M1 x)
    = M1 (gadjProof pcbf x)

instance GAdjProof b V1 where
  gadjProof _ _ = undefined

instance GAdjProof b U1 where
  {-# INLINE gadjProof #-}
  gadjProof _ u1 = u1

instance (GAdjProof b l, GAdjProof b r) => GAdjProof b (l :*: r) where
  {-# INLINE gadjProof #-}
  gadjProof pcbf (l :*: r)
    = (gadjProof pcbf l) :*: (gadjProof pcbf r)

instance (GAdjProof b l, GAdjProof b r) => GAdjProof b (l :+: r) where
  {-# INLINE gadjProof #-}
  gadjProof pcbf = \case
    L1 l -> L1 (gadjProof pcbf l)
    R1 r -> R1 (gadjProof pcbf r)


-- --------------------------------
-- The interesting cases
-- --------------------------------

instance {-# OVERLAPPING #-} GAdjProof b (K1 R (NonRec (Target F a))) where
  {-# INLINE gadjProof #-}
  gadjProof pcbf (K1 (NonRec fa))
    = K1 $ unsafeTarget @PxF (Pair (mkProof pcbf) $ unsafeUntarget @F fa)
    where
      mkProof :: c (f a) => Proxy (c (b f)) -> ProofOf c f a
      mkProof _ = proof

instance {-# OVERLAPPING #-} CanDeriveGenericInstance b => GAdjProof b (K1 R (RecUsage (b (Target F)))) where
  {-# INLINE gadjProof #-}
  gadjProof pcbf (K1 (RecUsage bf))
    = K1 $ to $ gadjProof pcbf $ fromWithRecAnn bf

instance {-# OVERLAPPING #-} ConstraintsB b' => GAdjProof b (K1 R (NonRec (b' (Target F)))) where
  {-# INLINE gadjProof #-}
  gadjProof pcbf (K1 (NonRec bf))
    = K1 $ unsafeTargetBarbie @PxF $ adjProof' pcbf $ unsafeUntargetBarbie @F bf
    where
      adjProof'
        :: ConstraintsOf c f b'
        => Proxy (c (b f)) -> b' f -> b' (Product (ProofOf c f) f)
      adjProof' _ = adjProof

instance (K1 i a) ~ Repl' (Target F) (Target PxF) (K1 i (NonRec a)) => GAdjProof b (K1 i (NonRec a)) where
  {-# INLINE gadjProof #-}
  gadjProof _ (K1 (NonRec a)) = K1 a
