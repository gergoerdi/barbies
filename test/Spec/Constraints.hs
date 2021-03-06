{-# LANGUAGE AllowAmbiguousTypes #-}
module Spec.Constraints
  ( lawAddDictPrj
  )

where

import Clothes(F)
import Barbies.Constraints(ClassF, Dict)
import Data.Functor.Barbie(bmap, ConstraintsB(..), AllBF)

import Data.Functor.Product (Product(Pair))
import Data.Typeable(Typeable, Proxy(..), typeRep)

import Test.Tasty(TestTree)
import Test.Tasty.QuickCheck(Arbitrary(..), testProperty, (===))


lawAddDictPrj
  :: forall b
  . ( ConstraintsB b, AllBF Show F b
    , Eq (b F)
    , Show (b F)
    , Arbitrary (b F)
    , Typeable b
    )
  => TestTree
lawAddDictPrj
  = testProperty (show (typeRep (Proxy :: Proxy b))) $ \b ->
      bmap second (baddDicts b :: b (Dict (ClassF Show F) `Product` F)) === b
  where
    second (Pair _ b) = b
