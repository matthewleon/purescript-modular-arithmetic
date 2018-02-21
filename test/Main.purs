module Test.Main where

import Prelude

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)
import Data.ModularArithmetic (class Prime, Z, genZ, inverse)
import Test.QuickCheck (class Arbitrary, Result, quickCheck, (<?>))
import Test.QuickCheck.Laws.Data (checkCommutativeRing, checkEuclideanRing, checkField, checkRing, checkSemiring)
import Type.Data.Nat (class Pos, D11)
import Type.Proxy (Proxy(..))

type Z11 = ArbitraryZ D11

main
  :: forall eff
   . Eff
     ( console :: CONSOLE
     , random :: RANDOM
     , exception :: EXCEPTION
     | eff )
     Unit
main = do
  let p = Proxy :: Proxy Z11

  quickCheck (inverses :: Z11 -> Result)

  checkSemiring p
  checkRing p
  checkCommutativeRing p
  checkEuclideanRing p
  checkField p

inverses :: forall m. (Prime m) => ArbitraryZ m -> Result
inverses (ArbitraryZ x) = go <?> "x: " <> show x
  where
    go
      | x /= zero =
          x * inverse x == one
      | otherwise =
          true

newtype ArbitraryZ m = ArbitraryZ (Z m)
derive newtype instance eqZ :: Eq (ArbitraryZ m)
derive newtype instance ordZ :: Ord (ArbitraryZ m)
derive newtype instance showZ :: Show (ArbitraryZ m)
derive newtype instance semiringZ :: Pos m => Semiring (ArbitraryZ m)
derive newtype instance ringZ :: Pos m => Ring (ArbitraryZ m)
derive newtype instance commutativeRingZ
  :: Pos m => CommutativeRing (ArbitraryZ m)
derive newtype instance euclideanRingZ
  :: Prime m => EuclideanRing (ArbitraryZ m)
derive newtype instance fieldZ :: Prime m => Field (ArbitraryZ m)
instance arbitraryZ :: Pos m => Arbitrary (ArbitraryZ m) where
  arbitrary = ArbitraryZ <$> genZ
