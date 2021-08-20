{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DerivingStrategies #-}

module Data.Vec.NatOps where
-- :load src/Data/Vec/NatOps.hs

-- imports:
import Data.Kind

-- imports: mine
import Data.Vec.Nat

-- ----------------------------------------------------------------------------
-- | Term-level Nat ops
-- ----------------------------------------------------------------------------
-- | Term-level Nat addition (recursive on the 1st param)
natAdd :: Nat -> Nat -> Nat
natAdd n m = case n of
    Z    -> m
    S n' -> S (natAdd n' m)

-- ----------------------------------------------------------------------------
-- | Type-level Nat ops
-- ----------------------------------------------------------------------------
-- | Type-level Nat addition (recursive on the 1st param)
type (+) :: Nat -> Nat -> Nat
type family n + m where
    Z      + m = m
    (S n') + m = S (n' + m)

-- | Type-level Nat addition (recursive on the 1st param)
type NatAdd :: Nat -> Nat -> Nat
type family NatAdd n m where
    NatAdd    Z   m = m
    NatAdd (S n') m = S (NatAdd n' m)

-- | Type-level Nat addition (recursive on the 2nd param)
type NatAdd2 :: Nat -> Nat -> Nat
type family NatAdd2 n m where
    NatAdd2 n    Z   = n
    NatAdd2 n (S m') = S (NatAdd2 n m')

-- ----------------------------------------------------------------------------
-- | Type-level Peano numbers: Min
-- The 'take' function on vectors won't work with equation (1) present. This is probably because, without the equation (1), NatMin and take have, mutatis mutandis, practically the same structure.
type family NatMin (n :: Nat) (m :: Nat) where
    -- NatMin    n     n  = n  (1)
    NatMin    Z     _  = Z
    NatMin (S _)    Z  = Z
    NatMin (S n) (S m) = S (NatMin n m)

{-
  take :: SNat m -> Vec n a -> Vec (NatMin m n) a
    take  SZ     _          = Nil
    take (SS _)  Nil        = Nil
    take (SS n) (Cons x xs) = Cons x (take n xs)
-}

-- ----------------------------------------------------------------------------
-- | Type-level Peano numbers: Modulus
type family (∸) (n :: Nat) (m :: Nat) where
    n      ∸    Z   = n
    Z      ∸    _   = Z
    (S n') ∸ (S m') = n' ∸ m'

-- ----------------------------------------------------------------------------
-- Relational ops
-- ----------------------------------------------------------------------------

-- | Type-level Peano numbers: Comparison
type family NatCmp (n :: Nat) (m :: Nat) where
    NatCmp    Z     Z  = 'EQ
    NatCmp    n     Z  = 'GT
    NatCmp    Z     m  = 'LT
    NatCmp (S n) (S m) = NatCmp n m

-- | Type-level Peano numbers: > (GT)
type (>) :: Nat -> Nat -> Bool
type family n > m where
    n     >    n  = 'False      -- (1)
    n     >    Z  = 'True       -- (2) n cannot be Z here
    Z     >    m  = 'False      -- (3)
    (S n) > (S m) = n > m       -- (4)
    -- n     >    m  = 'False   -- (5) keep either 3 or 5

-- ----------------------------------------------------------------------------
