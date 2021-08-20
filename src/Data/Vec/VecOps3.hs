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

module Data.Vec.VecOps3 where
-- :load src/Data/Vec/VecOps3.hs

-- imports:
import Data.Kind
import Prelude hiding
    ( (++)
    , take
    )

-- imports: mine
import Data.Vec.EVec
import Data.Vec.Nat
import Data.Vec.NatOps
import Data.Vec.SNat
import Data.Vec.Vec

-- ----------------------------------------------------------------------------
-- Vec functions, Part 3: Type Families
-- ----------------------------------------------------------------------------
{-
These vector ops are more involved.

Also, it became evident that each vector op that relies on a particular TF to work, forces that TF to have, mutatis mutandis, practically the same structure.

Some of these pairs:
* (++) and NatAdd
* take and NatMin

So, even if a TF works correctly on its own, adding a redundant equation, one that is not mirrored by a similar equation in the "associated" function on vectors, causes that function to error out.

In fact, even the choice of the parameter used for recursion matters: if (++) function recurses on the second param, then the NatAdd TF better do so as well. Moreover, this mirroring of structures between a particular TF function and the "associated" term-level function is so essential that even the commutativity of a TF (like NatAdd!) fails [related to the previous point].

-}

-- ----------------------------------------------------------------------------
-- Vec ops: (++)
-- ----------------------------------------------------------------------------
{-
    In the first equation, GHC learns that n ~ Z
    (coz the empty vec Nil has len Z),
    and comparing this against the NatAdd TF 1st equation:
    (NatAdd 'Z m = m), we get:
    n + m = Z + m = m

    In the 2nd equation, GHC learns that n ~ S n'
    and comparing this against the NatAdd TF 2nd equation:
    NatAdd ('S n) m = 'S (NatAdd n m), we get:
    n + m = (S n') + m = S (n' + m)

    The type of the bit on the RHS
    (x : xs)   :: Vec    n   a
    (x : xs)   :: Vec (S n') a     (n ~ S n')
    xs         :: Vec    n'  a
    ys         :: Vec    m   a
    (xs ++ ys) :: Vec (n' + m) a
    x ↑ (xs ++ ys) :: S (n' + m) = 1 + (n' + m)
-}
(++) :: Vec n a -> Vec m a -> Vec (n + m) a
Nil       ++ ys = ys
Cons x xs ++ ys = x ↑ (xs ++ ys)

-- ----------------------------------------------------------------------------
-- Vec ops: take
-- ----------------------------------------------------------------------------
{-
    List's 'take' allows overshooting the number of available elements, in which case it just returns the whole list. It also deals with negative integers by returning the empty list. We deal only with Nats, so we're ok in that regard.

        take :: Int -> [a] -> [a]
        take 0 _      = []
        take _ []     = []
        take n (x:xs) = x : take (n - 1) xs

        take 0 [0,1,2] == []
        take 2 [0,1,2] == [0,1]
        take 5 [0,1,2] == [0,1,2]

    At this point, the equation (2) is needed for exhaustive pattern matching, but, on the other hand, if it is present, the overall function errors!

        take :: SNat m -> Vec n a -> Vec m a
        take SZ               _ = Nil
        take (SS _)         Nil = Nil  (2)
        take (SS n) (Cons x xs) = Cons x (take n xs)

    The function fails coz (2) returns a (Vec Z a), despite us saying that the function always returns a Vec of length `m`, which is the same `m` as the input `SNat m`. To solve this we need the Min TF:

        type family NatMin (n :: Nat) (m :: Nat) where
            NatMin    n     n  = n  (1)
            NatMin    Z     _  = Z
            NatMin (S _)    Z  = Z
            NatMin (S n) (S m) = S (NatMin n m)

    The 'take' function on vectors won't work if NatMin TF keeps the (1) equation. This is probably because, without the equation (1), NatMin and 'take' have, mutatis mutandis, practically the same structure.
-}
take :: SNat m -> Vec n a -> Vec (NatMin m n) a
take SZ               _ = Nil
take (SS _)         Nil = Nil
take (SS n) (Cons x xs) = Cons x (take n xs)

-- ----------------------------------------------------------------------------
