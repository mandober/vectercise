{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies       #-}

module Data.Vec.FNat where
-- :load src/Data/Vec/FNat.hs

-- imports
import Data.Kind

-- imports: mine
import Data.Vec.Nat

-- ----------------------------------------------------------------------------
{-
We need a number to index vectors and impl (!!). The lists use Int, and than have to deal with negative numbers and overshoots. We could use a Nat as indexing type, which gets rid of the negatives but leaves the problem of overshooting the legth of the vector.

Vectors, as lists, are zero-indexed, so we have the usual mismatch that the 1st element is at position 0. The 3rd element of a 3 element vector is at index 2.

We'll use a custom GADTs called 'Fin' or 'FNat' to index vectors. FNat repr finite sets indexed by Nats.

FNat is a number that is always strictly less than the length of the vector. It should always holds that: Z <= FNat n < n , where `n` is the one in `Vec n a`

There's no indexing of empty vectors (for lists this entails an exception).
Vec of len 1 has only index 0 as the valid index.

(Vec n a) !! (FNat n) ~~> FNat n = [Z .. m] where n = S m

(Vec I   a) !! (FNat n) ~~> n = I   and FNat n = [FZ]
(Vec II  a) !! (FNat n) ~~> n = II  and FNat n = [FZ, FS FZ]
(Vec III a) !! (FNat n) ~~> n = III and FNat n = [FZ, FS FZ, FS (FS FZ)]

We have two data ctors, FZ (FZero) and FS (FSucc).
    type FNat :: Nat -> Type
    data FNat n where
        FZ :: FNat (S n)
        FS :: FNat n -> FNat (S n)

If (FNat n) is the index of vector, n in (Vec n a) better be larger.
If (FNat n) is the index of vector and it is FZ, n better be at least (S n).

-}

-- | Fin or FNat as finite sets indexed by finite numbers
type FNat :: Nat -> Type
data FNat n where
    FZ :: FNat (S n)
    FS :: FNat n -> FNat (S n)


deriving stock instance Show (FNat n)
deriving stock instance Eq   (FNat n)
deriving stock instance Ord  (FNat n)

-- ----------------------------------------------------------------------------
