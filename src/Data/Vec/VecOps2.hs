{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE DerivingStrategies       #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE RankNTypes               #-}
{-# LANGUAGE StandaloneDeriving       #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}

{-# OPTIONS_GHC -Wall                               #-}
{-# OPTIONS_GHC -Wno-unused-imports                 #-}
{-# OPTIONS_GHC -Wno-unused-top-binds               #-}
{-# OPTIONS_GHC -Wno-unticked-promoted-constructors #-}

module Data.Vec.VecOps2 where
-- :load src/Data/Vec/VecOps2.hs

-- imports:
import Data.Kind
import Prelude hiding
    ( length
    , replicate
    )

-- imports: mine
import Data.Vec.Nat
import Data.Vec.SNat
import Data.Vec.Vec

-- ----------------------------------------------------------------------------
-- Vec functions, Part 2: Singletons
-- ----------------------------------------------------------------------------
{-
  Data.List functions defined to work for Vec that are more complicated then those in the part 1. These require some special maneuvering to get right.

  https://www.youtube.com/watch?v=WHeBxSBY0fc&list=PLyzwHTVJlRc9QcF_tdqc9RdxJED8Mvyh1&index=27
-}

-- ----------------------------------------------------------------------------
-- length
-- ----------------------------------------------------------------------------
-- | (1) A naive length function that doesn't use Vec's true length. However, the returned Nat (although correct) is not guaranteed to be the Nat `n` in the supplied `Vec n a`. In fact, the type of the vector will be erased, so it won't be available at RT even if we had a way to somehow pull it down from the type level to the term level. It seems wasteful that we haul the length around, but cannot use it in O(1) when we really need it, but we must compute it in O(n).
length1 :: Vec n a -> Nat
length1 Nil = Z
length1 (Cons _ xs) = S (length1 xs)

-- | (2) A better approach is to use singletons SNats, that will be available at RT due to the isomorphism between their type and the single inhabiting term. Now that `length` returns an 'SNat n', it returns a RT representation of this CT type variable (type index) `n`. The singletons SNat grant us the bullit proof type-checking, so length cannot be calculated in an incorrect way, even on purpose. However, it's still done in O(n).
length :: forall (n :: Nat) a. Vec n a -> SNat n
length Nil = SZ
length (Cons _ xs) = SS (length xs)

-- ----------------------------------------------------------------------------
-- replicate
-- ----------------------------------------------------------------------------
-- replicate :: Int    -> a -> Vec n a  (sig for lists)
-- replicate :: Nat    -> a -> Vec n a  (naive sig for Vec)
-- replicate :: SNat n -> a -> Vec n a  (proper sig for Vec)

-- | This sig guarantees that the supplied nat in the form of a singleton `SNat n` is the same number that repr the length of the returned `Vec n a`.
replicate :: SNat n -> a -> Vec n a
replicate  SZ    _ = Nil
replicate (SS n) a = Cons a (replicate n a)

-- Note: When Dependent Haskell comes around, this will look like this:
-- replicate :: foreach (n :: Nat) -> a -> Vec n a
-- The `n` in foreach will exists both at CT and RT! Yey!
